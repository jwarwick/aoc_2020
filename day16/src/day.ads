-- AOC 2020, Day 16
with Ada.Containers.Vectors;
with Ada.Containers; use Ada.Containers;

package Day is
  type Tickets is private;

  function load_file(filename : in String) return Tickets;
  function sum_error_rate(t : in Tickets) return Natural;
  function departure_fields(t : in Tickets) return Long_Integer;

  private

  type Field_Rule is record
    Min, Max : Natural;
  end record;

  type Combined_Rules is array(1..2) of Field_Rule;

  package Rule_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Combined_Rules);
  use Rule_Vectors;

  package Value_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Natural);
  use Value_Vectors;

  package Nested_Vectors is new Ada.Containers.Vectors
    (Index_Type   => Natural,
    Element_Type => Value_Vectors.Vector);
  use Nested_Vectors;

  type Tickets is record
    Rules : Rule_Vectors.Vector := Rule_Vectors.Empty_Vector;
    Values : Nested_Vectors.Vector := Nested_Vectors.Empty_Vector;
    Ticket : Value_Vectors.Vector := Value_Vectors.Empty_Vector;
  end record;
end Day;
