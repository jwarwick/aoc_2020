-- AOC 2020, Day 11
with Ada.Containers.Ordered_Maps;

package Day is

  type Seat is (floor, occ, unocc);

  type Location is record
    x : Integer := 0;
    y : Integer := 0;
  end record;

  function "<"(Left, Right : in Location) return Boolean;

  package Seat_Map is new Ada.Containers.Ordered_Maps
     (Key_Type   => Location,
      Element_Type => Seat);
  use Seat_Map;

  type Ferry is record
    seats : Seat_Map.Map := Empty_Map;
    width : Natural := 0;
    height : Natural := 0;
  end record;

  function load_file(filename : in String) return Ferry;

  function steady_state_occupied(m : in Ferry) return Natural;
  function tolerant_steady_state_occupied(m : in Ferry) return Natural;
end Day;
