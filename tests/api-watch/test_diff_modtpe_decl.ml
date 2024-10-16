open Api_watch
open Test_helpers

let%expect_test "Module_type with value changes" =
  let reference =
    compile_interface
      {|
    module type M = sig
      val g : int -> string
    end
  |}
  in
  let current = compile_interface {|
    module type M 
  |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Module_type M: {Modified (Unsupported)}])}) |}]

let%expect_test "Module_type with module change" =
  let reference =
    compile_interface
      {|
    module type M = sig
      val b : int list -> int
    end
    module P : sig val x: float end
  |}
  in
  let current =
    compile_interface
      {|
    module type M = sig
      val b : float list -> float
    end
    module type N = sig val x: int end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {|
    Some (Module Main: {Modified (Supported [ Module P: Removed;
    Module M: {Modified (Supported [ Value (b, Modified)])};
    Module_type N: Added])})
    |}]

let%expect_test "Module_type to abstract module_type change" =
  let reference =
    compile_interface {|
  module type P = sig val x : int end
  |}
  in
  let current = compile_interface {|
  module type P
  |} in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Module_type P: {Modified (Unsupported)}])}) |}]

let%expect_test "Abstract module_type to module_type change" =
  let reference = compile_interface {|
  module type M
  |} in
  let current =
    compile_interface {|
  module type M = sig val x : int end
  |}
  in
  let result = Diff.interface ~module_name:"Main" ~reference ~current in
  Format.printf "%a" pp_diff_option result;
  [%expect
    {| Some (Module Main: {Modified (Supported [ Module_type M: {Modified (Unsupported)}])}) |}]