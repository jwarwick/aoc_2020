-- AoC 2020, Day 3
with Ada.Text_IO;
-- with Ada.Containers.Indefinite_Hashed_Maps;
-- with Ada.Containers.Ordered_Sets;
-- with Ada.Strings.Hash;
-- with Ada.Strings.Fixed; use Ada.Strings.Fixed;
-- with GNAT.String_Split;
-- with Ada.Characters.Latin_1;

package body Day is
  package TIO renames Ada.Text_IO;

  -- package Orbit_Element_Set is new Ada.Containers.Ordered_Sets
  --   (Element_Type => Unbounded_String);

  -- type Orbit is record
  --   Name : Unbounded_String := Null_Unbounded_String;
  --   Parent : Unbounded_String := Null_Unbounded_String;
  --   Children : Orbit_Element_Set.Set := Orbit_Element_Set.Empty_Set;
  --   Depth : Natural := 0;
  -- end record;

  -- pragma Warnings (Off, "procedure ""Put"" is not referenced");
  -- procedure Put(value : in Orbit) is
  -- pragma Warnings (On, "procedure ""Put"" is not referenced");
  -- begin
  --   TIO.Put("Orbit: " & to_string(value.Name) & ", Parent: " & to_string(value.Parent) & ", Depth: " & Natural'IMAGE(value.Depth));
  --   TIO.New_Line;
  --   TIO.Put("   Children: ");
  --   for c of value.Children loop
  --     TIO.Put(to_string(c) & " ");
  --   end loop;
  -- end Put;

  -- package Orbit_Hashed_Maps is new Ada.Containers.Indefinite_Hashed_Maps
  --    (Key_Type        => String,
  --     Element_Type    => Orbit,
  --     Hash            => Ada.Strings.Hash,
  --     Equivalent_Keys => "=");

  -- procedure parse_line(line : in String; orbits : in out Orbit_List.Vector) is
  --   idx : constant Natural := index(line, ")");
  --   left : constant String := line(line'first .. idx-1);
  --   right : constant String := line(idx+1 .. line'last);
  --   curr : constant Orbit_Entry := Orbit_Entry'(left => to_unbounded_string(left), right => to_unbounded_string(right));
  -- begin
  --   orbits.append(curr);
  -- end parse_line;

  function "="(left, right : in Position) return Boolean is
  begin
    return left.x = right.x and left.y = right.y;
  end "=";

  function "<"(left, right : in Position) return Boolean is
  begin
    if left.y < right.y then
      return true;
    elsif left.y = right.y and left.x < right.x then
      return true;
    else
      return true;
    end if;
  end "<";

  procedure parse_line(line : in String; y : in Natural; trees : in out Tree_Sets.Set) is
    x : Natural := 0;
  begin
    for c of line loop
      if c = '#' then
        trees.insert(Position'(X => x, Y => y));
      end if;
      x := x + 1;
    end loop;
  end parse_line;

  function load_map(filename : in String) return Forest is
    file : TIO.File_Type;
    trees : Tree_Sets.Set;
    height : Natural := 0;
    width : Natural := 0;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      declare
        l : constant String := TIO.get_line(file);
      begin
        parse_line(l, height, trees);
        width := l'Length;
      end;
      height := height + 1;
    end loop;
    TIO.close(file);
    return Forest'(Trees => trees, Width => width, Height => height);
  end load_map;

  function hit_tree(pos : in Position; trees : in Tree_Sets.Set) return Boolean is
  begin
    -- TIO.put_line("Trees:");
    -- for t of trees loop
    --   TIO.put_line(Natural'Image(t.x) & ", " & Natural'Image(t.y));
    -- end loop;
    -- TIO.put_line("X: " & Natural'Image(pos.x) & ", Y:" & Natural'Image(pos.y));
    -- TIO.put_line("    Hit: " & Boolean'Image(trees.contains(pos)));
    -- TIO.new_line;
    -- return trees.contains(pos);
    for t of trees loop
      if t = pos then
        return true;
      end if;
    end loop;
    return false;
  end hit_tree;

  function trees_hit(f : in Forest; slope : in Natural) return Natural is
    x : Natural := 0;
    hits : Natural := 0;
  begin
    for y in 0..(f.height-1) loop
      if hit_tree(Position'(X => x, Y => y), f.trees) then
        hits := hits + 1;
      end if;
      x := (x + slope) mod f.width;
    end loop;
    return hits;
  end trees_hit;

end Day;
