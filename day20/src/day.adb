-- AoC 2020, Day 20
with Ada.Text_IO;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Vectors;
use Ada.Containers;
with Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;

package body Day is
  package TIO renames Ada.Text_IO;

  type Line_Value is mod 2**10;
  type Tile_Array is array(0..9) of Line_Value;

  -- type Edge_Array is array(0..3) of Line_Value;

  type Configuration is (r1, r2, r3, r4);--, fr1, fr2, fr3, fr4);

  package Neighbor_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Natural);
  use Neighbor_Sets;

  type Tile is record
    id : Natural;

    neighbors : Neighbor_Sets.Set;

    config : Configuration;

    lines : Tile_Array;
    lines_rev : Tile_Array;
    top : Line_Value;
    top_rev : Line_Value;
    bottom : Line_Value;
    bottom_rev : Line_Value;
    left : Line_Value;
    left_rev : Line_Value;
    right : Line_Value;
    right_rev : Line_Value;
  end record;

  function id_hash(id: Natural) return Hash_Type is
  begin
    return Hash_Type(id);
  end id_hash;

  function line_hash(id: Line_Value) return Hash_Type is
  begin
    return Hash_Type(id);
  end line_hash;

  package Tile_Maps is new Ada.Containers.Indefinite_Hashed_Maps
    (Key_Type       => Natural,
    Element_Type    => Tile,
    Hash            => id_hash,
    Equivalent_Keys => "=");

  use Tile_Maps;
  tiles : Tile_Maps.Map := Empty_Map;

  -- function edges(t : in Tile) return Edge_Array is
  -- begin
  --   case t.config is
  --     when r1 => return (t.top, t.right, t.bottom, t.left);
  --     when r2 => return (t.left_rev, t.top, t.right_rev, t.bottom);
  --     when r3 => return (t.bottom_rev, t.left_rev, t.top_rev, t.right_rev);
  --     when r4 => return (t.right, t.bottom_rev, t.left, t.top_rev);
  --     -- when others => return (t.right, t.bottom_rev, t.left, t.top_rev);
  --     -- when fr1 => return (t.top_rev, t.left, t.bottom_rev, t.right);
  --     -- when fr2 => return (t.right_rev, t.top_rev, t.left_rev, t.bottom_rev);
  --     -- when fr3 => return (t.bottom, t.right_rev, t.top, t.left_rev);
  --     -- when fr4 => return (t.bottom_rev, t.right, t.top_rev, t.left);
  --   end case;
  -- end edges;

  procedure put_line(v : in Line_Value) is
  begin
    TIO.put_line(v'IMAGE);
    -- for i in reverse 0..9 loop
    --   if (v and 2**i) = 0 then
    --     TIO.put(".");
    --   else
    --     TIO.put("#");
    --   end if;
    -- end loop;
    -- TIO.new_line;
  end put_line;

  procedure put_line(t : in Tile) is
  begin
    TIO.put_line("Tile " & t.id'IMAGE & "=>");
    for l of t.lines loop
      put_line(l);
    end loop;
    TIO.put_line("Configuration: " & Configuration'IMAGE(t.config));
    TIO.put("top  =>");
    put_line(t.top);
    TIO.put("top' =>");
    put_line(t.top_rev);
    TIO.put("bottom  =>");
    put_line(t.bottom);
    TIO.put("bottom' =>");
    put_line(t.bottom_rev);
    TIO.put("left  =>");
    put_line(t.left);
    TIO.put("left' =>");
    put_line(t.left_rev);
    TIO.put("right  =>");
    put_line(t.right);
    TIO.put("right' =>");
    put_line(t.right_rev);
    TIO.new_line;
  end put_line;

  pragma Warnings (Off, "procedure ""put_line"" is not referenced");
  procedure put_line(input : in Tile_Maps.Map) is
  pragma Warnings (Off, "procedure ""put_line"" is not referenced");
  begin
    for t of input loop
      put_line(t);
      TIO.new_line;
    end loop;
  end put_line;

  function reverse_string(item : in String) return String is
      Result : String (Item'Range);
   begin
      for I in Item'range loop
         Result (Result'Last - I + Item'First) := Item (I);
      end loop;
      return Result;
   end reverse_string;

  procedure parse_tile(file : in out TIO.File_Type) is
    binary_map : constant Ada.Strings.Maps.Character_Mapping := Ada.Strings.Maps.To_Mapping(From => ".#", To => "01");

    line : constant String := TIO.get_line(file);
    id : constant Natural := Natural'Value(line(line'first+5..line'last-1));
    idx : Natural := 0;
    lines : Tile_Array;
    lines_rev : Tile_Array;
    left_str : Unbounded_String;
    right_str : Unbounded_String;
  begin
    loop
      declare
        line : constant String := TIO.get_line(file);
      begin
        if line'length = 0 then
          exit;
        end if;

        declare
          bin : constant String := Translate(line, binary_map);
          value : constant Line_Value := Line_Value'Value("2#" & bin & "#");
          value_rev : constant Line_Value := Line_Value'Value("2#" & reverse_string(bin) & "#");
        begin
          lines(idx) := value;
          lines_rev(idx) := value_rev;
          idx := idx + 1;
          left_str := left_str & bin(bin'first);
          right_str := right_str & bin(bin'last);
        end;

        if TIO.end_of_file(file) then
          exit;
        end if;
      end;
    end loop;
    declare
      top : constant Line_Value := lines(lines'first);
      top_rev : constant Line_Value := lines_rev(lines_rev'first);
      bottom : constant Line_Value := lines(lines'last);
      bottom_rev : constant Line_Value := lines_rev(lines_rev'last);
      left : constant Line_Value := Line_Value'Value("2#" & to_string(left_str) & "#");
      right : constant Line_Value := Line_Value'Value("2#" & to_string(right_str) & "#");
      left_rev : constant Line_Value := Line_Value'Value("2#" & reverse_string(to_string(left_str)) & "#");
      right_rev : constant Line_Value := Line_Value'Value("2#" & reverse_string(to_string(right_str)) & "#");
    begin
      tiles.insert(id, Tile'(id => id, neighbors => Neighbor_Sets.Empty_Set,
                             lines => lines, lines_rev => lines_rev, config => r1,
                             top => top, bottom => bottom,
                             left => left, right => right,
                             top_rev => top_rev, bottom_rev => bottom_rev,
                             left_rev => left_rev, right_rev => right_rev));
    end;
  end parse_tile;

  procedure load_tiles(filename : in String) is
    file : TIO.File_Type;
  begin
    tiles.clear;
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      parse_tile(file);
    end loop;
    TIO.close(file);
  end load_tiles;

  package Edge_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Line_Value);
  use Edge_Sets;

  package Tile_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Natural);
  use Tile_Vectors;

  function get_edges(id : in Natural) return Edge_Sets.Set is
    all_edges : Edge_Sets.Set := Edge_Sets.Empty_Set;
    t : constant Tile := tiles(id);
  begin
    all_edges.include(t.top);
    all_edges.include(t.top_rev);
    all_edges.include(t.bottom);
    all_edges.include(t.bottom_rev);
    all_edges.include(t.left);
    all_edges.include(t.left_rev);
    all_edges.include(t.right);
    all_edges.include(t.right_rev);
    return all_edges;
  end get_edges;

  -- function get_edges return Edge_Sets.Set is
  --   all_edges : Edge_Sets.Set := Edge_Sets.Empty_Set;
  -- begin
  --   for t of tiles loop
  --     declare
  --       es : constant Edge_Array := edges(t);
  --     begin
  --       for e of es loop
  --         all_edges.include(e);
  --       end loop;
  --     end;
  --   end loop;
  --   return all_edges;
  -- end get_edges;

  -- function get_edges_except(id : Natural) return Edge_Sets.Set is
  --   all_edges : Edge_Sets.Set := Edge_Sets.Empty_Set;
  -- begin
  --   for t of tiles loop
  --     if t.id /= id then
  --       declare
  --         es : constant Edge_Array := edges(t);
  --       begin
  --         for e of es loop
  --           all_edges.include(e);
  --         end loop;
  --       end;
  --     end if;
  --   end loop;
  --   return all_edges;
  -- end get_edges_except;

  procedure match is
    type Edge_Record is record
      id : Natural;
      conf : Configuration;
    end record;

    package Edge_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type       => Line_Value,
      Element_Type    => Edge_Record,
      Hash            => line_hash,
      Equivalent_Keys => "=");
    use Edge_Maps;

    edge_map : Edge_Maps.Map := Edge_Maps.Empty_Map;
  begin
    for c in tiles.Iterate loop
      declare
        t : Tile := tiles(c);
        es : constant Edge_Sets.Set := get_edges(t.id);
      begin
        for val of es loop
          if edge_map.contains(val) then
            declare
              matched_edge : constant Edge_Record := edge_map(val);
              matched_tile : Tile := tiles(matched_edge.id);
            begin
              if matched_edge.id /= t.id then
                TIO.put_line(val'IMAGE & "- Match: " & t.id'IMAGE & ", conf:" & t.config'IMAGE & " = " & matched_tile.id'IMAGE & ", conf: " & matched_edge.conf'IMAGE);

                matched_tile.neighbors.include(t.id); 
                tiles(matched_tile.id) := matched_tile;
                t.neighbors.include(matched_tile.id);
                tiles(c) := t;
              else
                TIO.put_line("BROKEN HERE?");
              end if;
            end;
          else
            TIO.put_line("Adding " & t.id'IMAGE & ", edge: " & val'IMAGE);
            edge_map.insert(val, Edge_Record'(id => t.id, conf => t.config));
          end if;
        end loop;
      end;
    end loop;
  end match;

  -- too slow for anything beyond the test input
  -- function permute_config(target : in Natural; ids : Tile_Vectors.Vector) return Boolean is
  -- begin
  --   if ids.is_empty then
  --     declare
  --       all_edges : constant Edge_Sets.Set := get_edges;
  --     begin
  --       return target = Natural(all_edges.length);
  --     end;
  --   end if;

  --   declare
  --     head : constant Natural := ids.first_element;
  --     rest : Tile_Vectors.Vector := ids;
  --     t : Tile := tiles(head);
  --   begin
  --     rest.delete_first;
  --     for c in Configuration loop
  --       t.config := c;
  --       tiles(head) := t;
  --       if permute_config(target, rest) then
  --         return true;
  --       end if;
  --     end loop;
  --     return false;
  --   end;
  -- end permute_config;

  function image_checksum(filename : in String) return Long_Integer is
    prod : Long_Integer := 1;
    num_tiles : Natural;
    side : Natural;
    expected : Natural;
    tile_ids : Tile_Vectors.Vector := Empty_Vector;
  begin
    load_tiles(filename);
    num_tiles := Natural(tiles.length);
    side := Natural(sqrt(Float(num_tiles)));
    expected := (num_tiles * 4) - (side * 4);

    for t of tiles loop
      tile_ids.append(t.id);
    end loop;

    TIO.put_line("Tiles: " & num_tiles'IMAGE);
    TIO.put_line("Side: " & side'IMAGE);
    TIO.put_line("Expected: " & expected'IMAGE);

    match;

    -- put_line(tiles);
    for t of tiles loop
      -- TIO.put_line("Tile: " & t.id'IMAGE);
      -- for n of t.neighbors loop
      --   TIO.put(n'IMAGE & ",");
      -- end loop;
      -- TIO.new_line;
      if t.neighbors.length = 2 then
        TIO.put_line("Found 2 edger: " & t.id'IMAGE);
        prod := prod * Long_Integer(t.id);
      end if;
    end loop;

    return prod;
  end image_checksum;
end Day;
