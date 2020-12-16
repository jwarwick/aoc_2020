-- AOC 2020, Day 16
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

package Day is
  type Tickets is private;

  function load_file(filename : in String) return Tickets;
  function sum_error_rate(t : in Tickets) return Natural;

  private

  type Field_Rule is record
    Min, Max : Natural;
  end record;

  package Rule_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Field_Rule);
  use Rule_Vectors;

  package Value_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Natural);
  use Value_Vectors;

  type Tickets is record
    Rules : Rule_Vectors.Vector := Rule_Vectors.Empty_Vector;
    Values : Value_Vectors.Vector := Value_Vectors.Empty_Vector;
  end record;
end Day;
