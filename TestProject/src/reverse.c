// Seamless C->Rust example: reversing a string

// Import types. More advanced compilers with generics wouldn't need
// to specify these generic args in theory, it would basically be a
// vanilla java import.
#pragma rsuse Str = &str
#pragma rsuse String = std::string::String
#pragma rsuse Chars = core::str::Chars<'static>
#pragma rsuse RevChars = core::iter::Rev<Chars>
// Import methods. AFAICT more advanced compilers with generics won't
// need any of these imports in theory.
#pragma rsfn Str_chars = str::chars
#pragma rsfn String_as_str = String::as_str
#pragma rsfn String_drop = String::drop
#pragma rsfn Chars_rev = Chars::rev
#pragma rsfn RevChars_collectString = RevChars::collect::<String>
// Import the header generated by the Makefile invoking the tool
#include <rust_deps/rust_deps.h>

#include <stdio.h>
#include <unistd.h>

int main() {
  // let str = "bork";
  Str str = rust_StrFromCStr("bork");
  // let iter = str.chars();
  Chars iter = Str_chars(str);
  // let rev_iter = iter.rev();
  RevChars revIter = Chars_rev(iter);
  // let rev_string: String = rev_iter.collect();
  String revString = RevChars_collectString(revIter);
  // let rev_str = rev_string.as_str();
  Str revStr = String_as_str(&revString);

  // Prints "krob"
  // Uses write() because Rust String isn't guaranteed null-terminated
  write(STDOUT_FILENO, rust_StrToCStr(revStr), rust_StrLen(revStr));
  printf("\nSuccess!\n");

  String_drop(revString);
  return 0;
}

// More notes:
// - rust_deps.h contains all the auto-generated bindings.
// - This is C, user needs to manage their memory. User shouldn't e.g.:
//   - Move `Str str` while Chars is alive.
//   - Forget to drop() the revString.
// - rust_StrFromCStr is a builtin that makes a Rust str from a char*.
// - rust_StrToCStr is a builtin that gets the char* back from Rust str.
// - rust_StrLen gets the length of a Rust str.