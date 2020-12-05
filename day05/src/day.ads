-- AOC 2020, Day 5
with Ada.Containers.Vectors;

package Day is
  type Row_Type is range 0..127;
  type Seat_Type is range 0..7;

  type Boarding_Pass is record
    Row : Row_Type := 0;
    Seat : Seat_Type := 0;
  end record;

  package Boarding_Pass_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Boarding_Pass);

  function load_batch(filename : in String) return Boarding_Pass_Vectors.Vector;
  function highest_id(passes : in Boarding_Pass_Vectors.Vector) return Natural;
  function missing_id(passes : in Boarding_Pass_Vectors.Vector) return Natural;
end Day;
