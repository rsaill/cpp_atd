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

let pp_list sep pp out lst =
  let rec aux first = function
    | [] -> ()
    | hd::tl -> 
    begin
      (if first then
        Printf.fprintf out "%a" pp hd
      else
        Printf.fprintf out "%s%a" sep pp hd);
      aux false tl
    end
  in
  aux true lst

let pp_enum (out:out_channel) (c:t_case) : unit =
  Printf.fprintf out "%s" c.c_name

let pp_subclass_decl (out:out_channel) (c:t_case) : unit =
  Printf.fprintf out "class %s;" c.c_name

let pp_field (out:out_channel) (f:t_field) : unit =
  Printf.fprintf out "const %s %s;" f.f_type f.f_name

let pp_init (out:out_channel) (f:t_field) : unit =
  Printf.fprintf out "%s{%s}" f.f_name f.f_name

let pp_arg (out:out_channel) (f:t_field) : unit =
  if f.f_by_ref then Printf.fprintf out "const %s &%s" f.f_type f.f_name
  else Printf.fprintf out "%s %s" f.f_type f.f_name

let pp_subclass_def (cl:string) (out:out_channel) (c:t_case) : unit =
  let aux out = function
    | [] -> ()
    | (_::_) as lst -> Printf.fprintf out ":%a" (pp_list ", " pp_init) lst
  in
  Printf.fprintf out
    "class %s::%s : public Abstract%s {
    public:
        %s(%a)%a{};
        void accept(Visitor &v) const { v.visit%s(*this); }
        Kind getKind() const { return Kind::%s; }
        %a
};" cl c.c_name cl c.c_name (pp_list "," pp_arg) c.c_fields  aux
    c.c_fields c.c_name c.c_name (pp_list "\n        " pp_field) c.c_fields

let pp_constructor (cl:string) (out:out_channel) (c:t_case) : unit =
  Printf.fprintf out "%s(const %s &e);" cl c.c_name

let pp_helper_decl (cl:string) (out:out_channel) (c:t_case) : unit =
  Printf.fprintf out "static %s make%s(%a);"
    cl c.c_name (pp_list ", " pp_arg) c.c_fields

let pp_cast (out:out_channel) (c:t_case) : unit =
  Printf.fprintf out "const %s& to%s() const;" c.c_name c.c_name

let pp_visitor_method (out:out_channel) (c:t_case) : unit =
  Printf.fprintf out "virtual void visit%s(const %s &e) = 0;" c.c_name c.c_name

let print_header (out:out_channel) (adt:adt) : unit =
  Printf.fprintf out
    "#ifndef %s_H
#define %s_H

#include <memory>

class %s {
public:
    enum class Kind { %a };
    %a
    %a
    Kind getKind() const;
    %a
    class Visitor {
        public:
        %a
    };
    void accept(Visitor &v) const;
    %a

private:
    class Abstract%s {
       public:
       virtual Kind getKind() const = 0;
       virtual void accept(Visitor &v) const = 0;
    };
    std::shared_ptr<Abstract%s> ptr;
};

%a

#endif
"
    (String.capitalize_ascii adt.name)
    (String.capitalize_ascii adt.name)
    adt.name
    (pp_list ", " pp_enum) adt.cases
    (pp_list "\n    " pp_subclass_decl) adt.cases
    (pp_list "\n    " (pp_constructor adt.name)) adt.cases
    (pp_list "\n    " pp_cast) adt.cases
    (pp_list "\n        " pp_visitor_method) adt.cases
    (pp_list "\n    " (pp_helper_decl adt.name)) adt.cases
    adt.name
    adt.name
    (pp_list "\n" (pp_subclass_def adt.name)) adt.cases

let pp_cast_def (cl:string) (out:out_channel) (c:t_case) : unit =
Printf.fprintf out "const %s::%s& %s::to%s() const{
  assert(getKind() == Kind::%s);
  return static_cast<%s&>(*ptr);
}" cl c.c_name cl c.c_name c.c_name c.c_name

let pp_constructor_def (cl:string) (out:out_channel) (c:t_case) : unit =
  Printf.fprintf out "%s::%s(const %s &e){ ptr = std::make_shared<%s>(e); };" cl cl c.c_name c.c_name

let pp_helper_def (cl:string) (out:out_channel) (c:t_case) : unit =
  let aux out f = Printf.fprintf out "%s" f.f_name in
  Printf.fprintf out "%s %s::make%s(%a){ return %s(%s(%a)); };"
    cl cl c.c_name (pp_list "," pp_arg) c.c_fields cl c.c_name
    (pp_list "," aux) c.c_fields

let print_implem (out:out_channel) (adt:adt) : unit =
  Printf.fprintf out
    "#include <cassert>
#include \"%s.h\"

%s::Kind %s::getKind() const { return ptr->getKind(); };
void %s::accept(Visitor &v) const { ptr->accept(v); };
%a
%a
%a"
    adt.filename
    adt.name
    adt.name
    adt.name
    (pp_list "\n" (pp_cast_def adt.name)) adt.cases
    (pp_list "\n" (pp_constructor_def adt.name)) adt.cases
    (pp_list "\n" (pp_helper_def adt.name)) adt.cases

let ex = {
  filename="term";
  name="Term";
  cases=[
    { c_name="Var"; c_fields=[{f_name="name"; f_type="std::string"; f_by_ref=true}]};
    { c_name="Lam"; c_fields=[{f_name="var"; f_type="std::string"; f_by_ref=true};{f_name="body"; f_type="Term"; f_by_ref=true}] };
    { c_name="App"; c_fields=[{f_name="fun";f_type="Term"; f_by_ref=true};{f_name="arg";f_type="Term"; f_by_ref=true}] }
  ];
}

let _ =
  let out_h = open_out (ex.filename ^ ".h") in
  let out_cpp = open_out (ex.filename ^ ".cpp") in
  print_header out_h ex;
  print_implem out_cpp ex
