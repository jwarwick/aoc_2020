-- AOC 2020, Day 13
with Ada.Containers.Vectors;

package Day is
  type Schedule is private;

  function load_file(filename : in String) return Schedule;
  function bus_mult(s : in Schedule) return Long_Long_Integer;
  function earliest_matching(s : in Schedule) return Long_Long_Integer;
  function earliest_matching_iterative(s : in Schedule) return Long_Long_Integer;

  private

  package Depart_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Long_Long_Integer);
  use Depart_Vectors;

  type Schedule is record
    earliest : Long_Long_Integer := 0;
    departures : Depart_Vectors.Vector := Empty_Vector;
    offsets : Depart_Vectors.Vector := Empty_Vector;
  end record;
end Day;
