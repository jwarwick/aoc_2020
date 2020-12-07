-- AoC 2020, Day 7
-- with Ada.Text_IO;
with Bag; use Bag;
with Ada.Containers;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Indefinite_Ordered_Maps;

package body Day is
  -- package TIO renames Ada.Text_IO;
  use Bag.Bag_Maps;

  package Color_Sets is new Ada.Containers.Ordered_Sets
    (Element_Type => Bag_Color);
  use Color_Sets;

  package Reverse_Bag_Maps is new Ada.Containers.Indefinite_Ordered_Maps
    (Key_Type        => Bag_Color,
    Element_Type    => Color_Sets.Set);
  use Reverse_Bag_Maps;

  function build_reverse_map(m : in Bag_Maps.Map) return Reverse_Bag_Maps.Map is
    rev : Reverse_Bag_Maps.Map;
  begin
    for c in m.Iterate loop
      for s of element(c) loop
        if not rev.contains(s.Color) then
          rev.include(s.Color, Empty_Set);
        end if;
        declare
          curr : Color_Sets.Set := rev(s.Color);
        begin
          curr.include(key(c));
          rev(s.color) := curr;
        end;
      end loop;
    end loop;
    return rev;
  end build_reverse_map;

  -- procedure print_rev(m : in Reverse_Bag_Maps.Map) is
  -- begin
  --   for c in m.Iterate loop
  --     TIO.put_line(Bag_Color'Image(key(c)));
  --     for s of element(c) loop
  --       TIO.put_line("    " & Bag_Color'Image(s));
  --     end loop;
  --   end loop;
  -- end print_rev;

  procedure dfs(curr : in Bag_Color; rev : in Reverse_Bag_Maps.Map; reach : in out Color_Sets.Set) is
    parents : Color_Sets.Set := Empty_Set;
  begin
    if rev.contains(curr) then
      parents := rev(curr);
      for p of parents loop
        if not reach.contains(p) then
          reach.insert(p);
          dfs(p, rev, reach);
        end if;
      end loop;
    end if;
  end dfs;

  function count_valid(m : in Bag_Maps.Map) return Natural is
    rev : constant Reverse_Bag_Maps.Map := build_reverse_map(m);
    reach : Color_Sets.Set := Empty_Set;
  begin
    -- print_rev(rev);
    dfs(Bag.my_color, rev, reach);
    return Natural(Color_Sets.length(reach));
  end count_valid;

  function valid_bag_colors return Natural is
    b : constant Bag_Maps.Map := Bag.input_data;
  begin
    return count_valid(b);
  end valid_bag_colors;

  function valid_test_bag_colors return Natural is
    b : constant Bag_Maps.Map := Bag.test_data;
  begin
    return count_valid(b);
  end valid_test_bag_colors;

  function count_nested(curr : in Bag_Color; bags : in Bag_Maps.Map) return Natural is
    subs : Bag_Sets.Set;
    sum : Natural := 0;
  begin
    if bags.contains(curr) then
      subs := bags(curr);
      for s of subs loop
        sum := sum + s.count + (s.count * count_nested(s.color, bags));
      end loop;
      return sum;
    else
      return 0;
    end if;
  end count_nested;

  function nested_bags return Natural is
    b : constant Bag_Maps.Map := Bag.input_data;
  begin
    return count_nested(my_color, b);
  end nested_bags;

  function nested_test_bags return Natural is
    b : constant Bag_Maps.Map := Bag.test_data;
  begin
    return count_nested(my_color, b);
  end nested_test_bags;
end Day;
