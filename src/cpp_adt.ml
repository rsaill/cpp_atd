type t_field =
  { f_name: string;
    f_type: string;
    f_by_ref: bool;
  }
type t_case =
  { c_name: string;
    c_fields: t_field list;
  }
type adt = {
  filename: string;
  name: string;
  cases: t_case list;
}

let header_template : Mustache.t =
  Mustache.of_string 
    "#ifndef {{caps_name}}_H
#define {{caps_name}}_H

#include <memory>
#include <cassert>

class {{name}} {
    public:
        enum class Kind { {{#cases}}{{#c_comma}}, {{/c_comma}}{{c_name}}{{/cases}} };
    
        Kind getKind() const;
    
        {{#cases}}
        class {{c_name}};
        {{/cases}}
    
        {{#cases}}
        {{name}}(const {{c_name}} &e);
        {{/cases}}
    
        {{#cases}}
        const {{c_name}}& to{{c_name}}() const;
        {{/cases}}
    
        {{#cases}}
        static {{name}} make{{c_name}}({{#fields}}{{#f_comma}}, {{/f_comma}}{{{f_type2}}} {{f_name}}{{/fields}});
        {{/cases}}
    
        class Visitor {
            public:
                {{#cases}}
                virtual void visit{{c_name}}(const {{c_name}} &e) = 0;
                {{/cases}}
        };
        void accept(Visitor &v) const;

        inline bool operator==(const {{name}} &other) const;
        
    
    private:
        class Abstract{{name}} {
            public:
                virtual Kind getKind() const = 0;
                virtual void accept(Visitor &v) const = 0;
        };
        std::shared_ptr<Abstract{{name}}> ptr;
};
{{#cases}}

class {{name}}::{{c_name}} : public Abstract{{name}} {
    public:
        {{c_name}}({{#fields}}{{#f_comma}}, {{/f_comma}}{{{f_type2}}} {{f_name}}{{/fields}})
            :{{#fields}}{{#f_comma}}, {{/f_comma}}{{f_name}}{ {{f_name}} }{{/fields}}
        {};
        void accept(Visitor &v) const { v.visit{{c_name}}( *this ); }
        Kind getKind() const { return Kind::{{c_name}}; }
        bool operator==(const {{c_name}} &other) const {
        {{#fields}}{{#f_first}}    return {{/f_first}}{{^f_first}} && {{/f_first}}{{f_name}} == other.{{f_name}}{{/fields}};
        };
        {{#fields}}
        const {{f_type}} {{f_name}};
        {{/fields}}
};
{{/cases}}

#endif
"

let implem_template : Mustache.t =
  Mustache.of_string 
    "#include <cassert>
#include \"{{filename}}.h\"

{{name}}::Kind {{name}}::getKind() const { return ptr->getKind(); };

void {{name}}::accept(Visitor &v) const { ptr->accept(v); };

{{#cases}}
const {{name}}::{{c_name}}& {{name}}::to{{c_name}}() const {
  assert(getKind() == Kind::{{c_name}});
  return static_cast<{{c_name}}&>(*ptr);
}
{{/cases}}

{{#cases}}
{{name}}::{{name}}(const {{c_name}} &e){ ptr = std::make_shared<{{c_name}}>(e); };
{{/cases}}

{{#cases}}
{{name}} {{name}}::make{{c_name}}({{#fields}}{{#f_comma}}, {{/f_comma}}{{{f_type2}}} {{f_name}}{{/fields}}){
  return {{name}}({{c_name}}({{#fields}}{{#f_comma}},{{/f_comma}}{{f_name}}{{/fields}}));
};
{{/cases}}

bool {{name}}::operator==(const {{name}} &other) const {
    if(getKind() != other.getKind())
        return false;

    switch(getKind()){
    {{#cases}}
        case Kind::{{c_name}}:
            return to{{c_name}}() == other.to{{c_name}}();
    {{/cases}}
    }
    assert(false); // unreachable
}"

let ex = {
  filename="term";
  name="Term";
  cases=[
    { c_name="Var"; c_fields=[{f_name="name"; f_type="std::string"; f_by_ref=true}]};
    { c_name="Lam"; c_fields=[{f_name="var"; f_type="std::string"; f_by_ref=true};{f_name="body"; f_type="Term"; f_by_ref=true}] };
    { c_name="App"; c_fields=[{f_name="fun";f_type="Term"; f_by_ref=true};{f_name="arg";f_type="Term"; f_by_ref=true}] }
  ];
}
let to_json (adt:adt) =
  let cases =
    List.mapi (fun i case -> 
        let fields = List.mapi (fun i fd ->
            `O [ "f_name", `String fd.f_name
               ; "f_type", `String fd.f_type
               ; "f_type2", `String
                   (if fd.f_by_ref then ("const " ^ fd.f_type ^ "&") else fd.f_type)
               ; "f_first", `Bool (i == 0)
               ; "f_comma", `Bool (i <> 0) ]
          ) case.c_fields in
        `O [ "c_name",`String case.c_name
           ; "c_comma", `Bool (i <> 0)
           ; "fields", `A fields
           ]
      ) adt.cases
  in
  `O [ "name", `String adt.name 
     ; "caps_name", `String (String.capitalize_ascii adt.name) 
     ; "filename", `String adt.filename 
     ; "cases", `A cases
     ]

let _ =
  let out_h = Format.formatter_of_out_channel (open_out (ex.filename ^ ".h")) in
  let out_cpp = Format.formatter_of_out_channel (open_out (ex.filename ^ ".cpp")) in
  let json = to_json ex in
  Mustache.render_fmt out_h header_template json;
  Mustache.render_fmt out_cpp implem_template json
