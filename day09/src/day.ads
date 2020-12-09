-- AOC 2020, Day 9
with Ada.Containers.Vectors;

package Day is

  package XMAS_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Long_Integer);
  use XMAS_Vector;

  function load_file(filename : in String) return XMAS_Vector.Vector;

  function first_invalid(v : in XMAS_Vector.Vector; preamble : in Positive) return Long_Integer;
end Day;
