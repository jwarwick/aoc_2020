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
    v : constant Long_Integer := Long_Integer((h.x * 17) + (h.y * 29) + (h.z * 43));
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

  function count_tiles(filename : in String) return Natural is
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
    return Natural(h.length);
  end count_tiles;
end Day;
