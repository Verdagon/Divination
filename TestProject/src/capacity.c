// Import types
#pragma rsuse VecInt = std::vec::Vec<u64>
// Import methods (theoretically only needed for non-generic languages like C)
#pragma rsfn VecInt_with_capacity = VecInt::with_capacity
#pragma rsfn VecInt_capacity = VecInt::capacity
#pragma rsfn VecInt_drop = VecInt::drop

// Import the header generated by the Makefile invoking the tool
#include "rust_deps/rust_deps.h"

#include <stdio.h>

int main() {
  VecInt argv = VecInt_with_capacity(42);
  printf("Capacity: %lu\n", VecInt_capacity(&argv));
  VecInt_drop(argv);
  return 0;
}
