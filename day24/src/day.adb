-- AoC 2020, Day 24
with Ada.Text_IO;
with Ada.Containers.Hashed_Sets;
use Ada.Containers;

package body Day is
  package TIO renames Ada.Text_IO;

  type Hex is record
    x, y, z : Integer;
  end record;

  function hex_hash(h : in Hex) return Hash_Type is
    v : constant Long_Integer := Long_Integer((h.x * 103) + (h.y * 29) + (h.z * 43));
  begin
    return Hash_Type(v mod Long_Integer(Hash_Type'Last));
  end hex_hash;

  package Hex_Sets is new Ada.Containers.Hashed_Sets
    (Element_Type        => Hex,
    Hash                => hex_hash,
    Equivalent_Elements => "=");
  use Hex_Sets;

  procedure move_east(h : in out Hex) is
  begin
    h.x := h.x + 1;
    h.y := h.y - 1;
  end move_east;

  procedure move_west(h : in out Hex) is
  begin
    h.x := h.x - 1;
    h.y := h.y + 1;
  end move_west;

  procedure move_north(h : in out Hex; dir : in Character) is
  begin
    h.z := h.z - 1;
    if dir = 'e' then
      h.x := h.x + 1;
    else
      h.y := h.y + 1;
    end if;
  end move_north;

  procedure move_south(h : in out Hex; dir : in Character) is
  begin
    h.z := h.z + 1;
    if dir = 'e' then
      h.y := h.y - 1;
    else
      h.x := h.x - 1;
    end if;
  end move_south;

  type Color_Type is (black, white);

  function color(c : in Hex; h : in Hex_Sets.set) return Color_Type is
  begin 
    if h.contains(c) then
      return black;
    else
      return white;
    end if;
  end color;

  function neighbors(c : in Hex) return Hex_Sets.Set is
    n : Hex_Sets.Set := Empty_Set;
  begin
    n.include(Hex'(x => c.x+1, y => c.y, z => c.z-1));
    n.include(Hex'(x => c.x+1, y => c.y-1, z => c.z));
    n.include(Hex'(x => c.x, y => c.y-1, z => c.z+1));
    n.include(Hex'(x => c.x-1, y => c.y, z => c.z+1));
    n.include(Hex'(x => c.x-1, y => c.y+1, z => c.z));
    n.include(Hex'(x => c.x, y => c.y+1, z => c.z-1));
    return n;
  end neighbors;

  function all_neighbors(h : in Hex_Sets.set) return Hex_Sets.Set is
    n : Hex_Sets.Set := Empty_Set;
  begin
    n.reserve_capacity(10_000);
    for c of h loop
      n := n or neighbors(c);
    end loop;
    return n;
  end all_neighbors;

  -- procedure put(h : in Hex) is
  -- begin
  --   TIO.put("(" & h.x'IMAGE & "," & h.y'IMAGE & "," & h.z'IMAGE & ")");
  -- end put;

  function black_neighbors(c : in Hex; h : in Hex_Sets.Set) return Natural is
    neighs : constant Hex_Sets.Set := neighbors(c);
    total : Natural := 0;
  begin
    for c of neighs loop
      if black = color(c, h) then
        total := total + 1;
      end if;
    end loop;
    return total;
  end black_neighbors;

  procedure step(h : in out Hex_Sets.set) is
    n : constant Hex_Sets.Set := all_neighbors(h);
    complete : constant Hex_Sets.Set := h or n;
    next : Hex_Sets.Set := Empty_Set;
  begin
    next.reserve_capacity(10_000);
    TIO.put_line("Count: " & complete.length'IMAGE);
    for c of complete loop
      declare
        black_count : constant Natural := black_neighbors(c, h);
      begin
        case color(c, h) is
          when white =>
            if black_count = 2 then
              next.include(c);
            end if;
          when black =>
            if not(black_count = 0 or black_count > 2) then
              next.include(c);
            end if;
        end case;
      end;
    end loop;
    h := next;
  end step;

  function eval_input(filename : in String) return Hex_Sets.Set is
    file : TIO.File_Type;
    h : Hex_Sets.Set := Empty_Set;
    start_hex : constant Hex := Hex'(x=>0, y=>0, z=>0);
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      declare
        line : constant String := TIO.get_line(file);
        curr : Hex := start_hex;
        last_char : Character := ' ';
      begin
        for c of line loop
          if last_char = 's' then
            move_south(curr, c);
            last_char := ' ';
          elsif last_char = 'n' then
            move_north(curr, c);
            last_char := ' ';
          else
            case c is
              when 'e' => move_east(curr);
              when 'w' => move_west(curr);
              when others => last_char := c;
            end case;
          end if;
        end loop;
        if h.contains(curr) then
          h.delete(curr);
        else
          h.insert(curr);
        end if;
      end;
    end loop;
    TIO.close(file);
    return h;
  end eval_input;

  function count_tiles(filename : in String) return Natural is
    h : constant Hex_Sets.Set := eval_input(filename);
  begin
    return Natural(h.length);
  end count_tiles;

  function evolve_tiles(filename : in String; steps : in Natural) return Natural is
    h : Hex_Sets.Set := eval_input(filename);
  begin
    h.reserve_capacity(10_000);
    for s in 1..steps loop
      step(h);
      TIO.put_line("Day" & s'IMAGE & ":" & h.length'IMAGE);
    end loop;
    return Natural(h.length);
  end evolve_tiles;
end Day;
