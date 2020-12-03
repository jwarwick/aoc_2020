-- AoC 2020, Day 3
with Ada.Text_IO;

package body Day is
  package TIO renames Ada.Text_IO;
  function "<"(left, right : in Position) return Boolean is
  begin
    if left.y < right.y then
      return true;
    elsif left.y = right.y and left.x < right.x then
      return true;
    else
      return false;
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
    return trees.contains(pos);
  end hit_tree;

  function trees_hit_slope(f : in Forest; slope_x : in Natural; slope_y : in Natural) return Natural is
    x : Natural := 0;
    y : Natural := 0;
    hits : Natural := 0;
  begin
    while y < f.height loop
      if hit_tree(Position'(X => x, Y => y), f.trees) then
        hits := hits + 1;
      end if;
      x := (x + slope_x) mod f.width;
      y := y + slope_y;
    end loop;
    return hits;
  end trees_hit_slope;

  function trees_hit(f : in Forest; slope : in Natural) return Natural is
  begin
    return trees_hit_slope(f, slope, 1);
  end trees_hit;

  function many_trees_hit(f : in Forest) return Natural is
    mult : Natural := 1;
  begin
    mult := trees_hit_slope(f, 1, 1);
    mult := mult * trees_hit_slope(f, 3, 1);
    mult := mult * trees_hit_slope(f, 5, 1);
    mult := mult * trees_hit_slope(f, 7, 1);
    mult := mult * trees_hit_slope(f, 1, 2);
    return mult;
  end many_trees_hit;
end Day;
