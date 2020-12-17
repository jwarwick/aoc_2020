-- AOC 2020, Day 17
with Ada.Containers.Hashed_Sets;
use Ada.Containers;

package Day is

  type Location is record
    x : Integer := 0;
    y : Integer := 0;
    z : Integer := 0;
  end record;

  function location_hash(key : in Location) return Hash_Type;
  
  package Grid_Set is new Ada.Containers.Hashed_Sets
     (Element_Type => Location,
      Hash => location_hash,
      Equivalent_Elements => "=");
  use Grid_Set;

  function load_file(filename : in String) return Grid_Set.Set;
  function active_count(g : in Grid_Set.Set; cycles : in Natural) return Natural;
end Day;
