-- AoC 2020, Day 17
with Ada.Text_IO;

package body Day is
  package TIO renames Ada.Text_IO;

  function location_hash(key : in Location) return Hash_Type is
  begin
    return Hash_Type(abs((key.x * 137) + (key.y * 149) + (key.z * 163) + (key.w * 13))); 
  end location_hash;

  procedure parse_line(line : in String; y : in Natural; g : in out Grid_Set.Set) is
    x : Natural := 0;
  begin
    for c of line loop
      if c = '#' then
        g.insert(Location'(X => x, Y => y, Z=> 0, W => 0));
      end if;
      x := x + 1;
    end loop;
  end parse_line;

  function load_file(filename : in String) return Grid_Set.Set is
    file : TIO.File_Type;
    g : Grid_Set.Set := Empty_Set;
    y : Natural := 0;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      parse_line(TIO.get_line(file), y, g);
      y := y + 1;
    end loop;
    TIO.close(file);
    return g;
  end load_file;

  pragma Warnings (Off);
  procedure put_line(l : in Location) is
    pragma Warnings (On);
  begin
    TIO.put_line(l.x'IMAGE & "," & l.y'IMAGE & "," & l.z'IMAGE & "," & l.w'IMAGE);
  end put_line;
  
  function neighbors(l : in Location) return Grid_Set.Set is
    g : Grid_Set.Set := Empty_Set;
  begin
    for x in l.x-1 .. l.x+1 loop
      for y in l.y-1 .. l.y+1 loop
        for z in l.z-1 .. l.z+1 loop
            g.include(Location'(X=>x, Y=>y, Z=>z, W=>0));
        end loop;
      end loop;
    end loop;
    g.exclude(l);
    return g;
  end neighbors;

  function neighbors_4d(l : in Location) return Grid_Set.Set is
    g : Grid_Set.Set := Empty_Set;
  begin
    for x in l.x-1 .. l.x+1 loop
      for y in l.y-1 .. l.y+1 loop
        for z in l.z-1 .. l.z+1 loop
          for w in l.w-1 .. l.w+1 loop
            g.include(Location'(X=>x, Y=>y, Z=>z, W=>w));
          end loop;
        end loop;
      end loop;
    end loop;
    g.exclude(l);
    return g;
  end neighbors_4d;

  function active(to_check : in Grid_Set.Set; active : in Grid_Set.Set) return Natural is
    cnt : Natural := 0;
  begin
    for l of to_check loop
      if active.contains(l) then
        cnt := cnt + 1;
      end if;
    end loop;
    return cnt;
  end active;

  procedure step(g : in out Grid_Set.Set; fourd : in Boolean) is
    old_grid : constant Grid_Set.Set := g;
    new_grid : Grid_Set.Set := Empty_Set;
    all_possible : Grid_Set.Set := Empty_Set;
  begin
    for p of old_grid loop
      all_possible.include(p);
      if fourd then
        all_possible := all_possible or neighbors_4d(p);
      else
        all_possible := all_possible or neighbors(p);
      end if;
    end loop;

    for p of all_possible loop
      declare
        neighs : Grid_Set.Set := Empty_Set;
        active_neigh : Natural := 0;
        was_active : constant Boolean := old_grid.contains(p);
      begin
        if fourd then
          neighs := neighbors_4d(p);
        else
          neighs := neighbors(p);
        end if;
        active_neigh := active(neighs, old_grid);

        if was_active then
          if active_neigh = 2 or active_neigh = 3 then
            new_grid.include(p);
          end if;
        else
          if active_neigh = 3 then
            new_grid.include(p);
          end if;
        end if;
      end;
    end loop;

    g := new_grid;
  end step;

  function active_count(g : in Grid_Set.Set; cycles : in Natural) return Natural is
    grid : Grid_Set.Set := g;
  begin
    for i in 1..cycles loop
      step(grid, false);
    end loop;
    return Natural(grid.length);
  end active_count;

  function active_count_4d(g : in Grid_Set.Set; cycles : in Natural) return Natural is
    grid : Grid_Set.Set := g;
  begin
    for i in 1..cycles loop
      step(grid, true);
    end loop;
    return Natural(grid.length);
  end active_count_4d;
end Day;
