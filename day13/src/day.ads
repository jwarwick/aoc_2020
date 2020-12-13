-- AOC 2020, Day 13
with Ada.Containers.Vectors;

package Day is
  type Schedule is private;

  function load_file(filename : in String) return Schedule;
  function bus_mult(s : in Schedule) return Natural;

  private

  package Depart_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Natural);
  use Depart_Vectors;

  type Schedule is record
    earliest : Natural := 0;
    departures : Depart_Vectors.Vector := Empty_Vector;
  end record;
end Day;
