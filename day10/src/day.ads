-- AOC 2020, Day 10
with Ada.Containers.Vectors;

package Day is

  package Adaptors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Natural);
  use Adaptors;

  function load_file(filename : in String) return Adaptors.Vector;

  function mult_1_3_differences(v : in Adaptors.Vector) return Natural;
  function total_arrangments(v : in Adaptors.Vector) return Long_Integer;
end Day;
