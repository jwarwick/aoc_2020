-- AOC 2020, Day 3
with Ada.Containers.Ordered_Sets;

package Day is
  type Position is Record
    X : Natural := 0;
    Y : Natural := 0;
  end record;

  function "<"(left, right : in Position) return Boolean;

  package Tree_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Position);

  type Forest is Record
    Trees : Tree_Sets.Set;
    height : Natural := 0;
    width : Natural := 0;
  end record;

  function load_map(filename : in String) return Forest;
  function trees_hit(f : in Forest; slope : in Natural) return Natural;
  function many_trees_hit(f : in Forest) return Natural;
end Day;
