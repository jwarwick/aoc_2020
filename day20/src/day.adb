-- AoC 2020, Day 20
with Ada.Text_IO;
with Ada.Numerics.Elementary_Functions;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Vectors;
use Ada.Containers;
with Ada.Strings.Maps;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Day is
  package TIO renames Ada.Text_IO;

  type Line_Value is mod 2**10;
  type Tile_Array is array(0..9) of Line_Value;

  type Image_Array is array(0..9, 0..9) of Boolean;

  type Position is record
    x, y : Natural;
  end record;

  function position_hash(p : in Position) return Hash_Type is
  begin
    return Hash_Type(p.x * 79 + p.y * 53);
  end position_hash;

  package Complete_Image_Sets is new Ada.Containers.Hashed_Sets
    (Element_Type => Position,
    Hash => position_hash,
    Equivalent_Elements => "=");
  use Complete_Image_Sets;

  type Configuration is (r1, r2, r3, r4, fr1, fr2, fr3, fr4);

  package Neighbor_Sets is new Ada.Containers.Ordered_Sets
     (Element_Type => Natural);
  use Neighbor_Sets;

  type Tile is record
    id : Natural;

    neighbors : Neighbor_Sets.Set;

    config : Configuration;
    image : Image_Array;

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

  pragma Warnings (Off, "procedure ""put_line"" is not referenced");
  procedure put_line(i : in Image_Array) is
  pragma Warnings (On, "procedure ""put_line"" is not referenced");
  begin
    for y in 0..9 loop
      for x in 0..9 loop
        if i(x, y) then
          TIO.put("#");
        else
          TIO.put(".");
        end if;
      end loop;
      TIO.new_line;
    end loop;
  end put_line;

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
    image : Image_Array;
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
          for c in line'Range loop
            image(c-1, idx) := line(c) = '#';
          end loop;
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
                             image => image,
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
              matched_tile.neighbors.include(t.id); 
              tiles(matched_tile.id) := matched_tile;
              t.neighbors.include(matched_tile.id);
              tiles(c) := t;
            end;
          else
            edge_map.insert(val, Edge_Record'(id => t.id, conf => t.config));
          end if;
        end loop;
      end;
    end loop;
  end match;

  type Edge_Array is array(0..3) of Line_Value;
  function edges(id : in Natural; orientation : in Configuration) return Edge_Array is
    t : constant Tile := tiles(id);
  begin
    case orientation is
      when r1 => return (t.top, t.right, t.bottom, t.left);
      when r2 => return (t.left_rev, t.top, t.right_rev, t.bottom);
      when r3 => return (t.bottom_rev, t.left_rev, t.top_rev, t.right_rev);
      when r4 => return (t.right, t.bottom_rev, t.left, t.top_rev);
      when fr1 => return (t.top_rev, t.left, t.bottom_rev, t.right);
      when fr2 => return (t.right_rev, t.top_rev, t.left_rev, t.bottom_rev);
      when fr3 => return (t.bottom, t.right_rev, t.top, t.left_rev);
      when fr4 => return (t.bottom_rev, t.right, t.top_rev, t.left);
    end case;
  end edges;

  function flip(i : in Image_Array) return Image_Array is
    result : Image_Array;
  begin
    for y in 0..9 loop
      for x in 0..9 loop
        result(x, y) := i(9-x, y);
      end loop;
    end loop;
    return result;
  end flip;

  -- rotate 90deg clockwise
  function rotate(i : in Image_Array) return Image_Array is
    result : Image_Array;
  begin
    for y in 0..9 loop
      for x in 0..9 loop
        result(x, y) := i(y, 9-x);
      end loop;
    end loop;
    return result;
  end rotate;

  function transform(i : in Image_Array; orientation : in Configuration) return Image_Array is
  begin
    case orientation is
      when r1 => return i;
      when r2 => return rotate(i);
      when r3 => return rotate(rotate(i));
      when r4 => return rotate(rotate(rotate(i)));
      when fr1 => return flip(i);
      when fr2 => return rotate(flip(i));
      when fr3 => return rotate(rotate(flip(i)));
      when fr4 => return rotate(rotate(rotate(flip(i))));
    end case;
  end transform;

  complete_image : Complete_Image_Sets.Set := Complete_Image_Sets.Empty_Set;

  procedure update_complete_image(i : in Image_Array; x_offset : in Natural; y_offset : in Natural) is
  begin
    for y in 1..8 loop
      for x in 1..8 loop
        if i(x, y) then
          declare
            nx : constant Natural := (x_offset * 8) + (x-1);
            ny : constant Natural := (y_offset * 8) + (y-1);
          begin
            complete_image.insert(Position'(x => nx, y=> ny));
          end;
        end if;
      end loop;
    end loop;
  end update_complete_image;

  -- rotate 90deg clockwise
  procedure rotate_complete_image is
    max : constant Natural := Natural(Ada.Numerics.Elementary_Functions.sqrt(Float(tiles.length)) * 8.0) - 1;
    new_image : Complete_Image_Sets.Set := Complete_Image_Sets.Empty_Set;
  begin
    for pixel of complete_image loop
      new_image.insert(Position'(x=>pixel.y, y=>max-pixel.x));
    end loop;
    complete_image := new_image;
  end rotate_complete_image;

  procedure flip_complete_image is
    max : constant Natural := Natural(Ada.Numerics.Elementary_Functions.sqrt(Float(tiles.length)) * 8.0) - 1;
    new_image : Complete_Image_Sets.Set := Complete_Image_Sets.Empty_Set;
  begin
    for pixel of complete_image loop
      new_image.insert(Position'(x=>max-pixel.x, y=>pixel.y));
    end loop;
    complete_image := new_image;
  end flip_complete_image;

  pragma Warnings (Off, "procedure ""put_line"" is not referenced");
  procedure put_line(i : in Complete_Image_Sets.Set) is
  pragma Warnings (On, "procedure ""put_line"" is not referenced");
    max : constant Natural := Natural(Ada.Numerics.Elementary_Functions.sqrt(Float(tiles.length)) * 8.0) - 1;
  begin
    for y in 0..max loop
      for x in 0..max loop
        if i.contains(Position'(x=>x, y=>y)) then
          TIO.put("#");
        else
          TIO.put(".");
        end if;
      end loop;
      TIO.new_line;
    end loop;
  end put_line;

  function dragon_pixel(x, y : in Natural) return Boolean is
  begin
    return complete_image.contains(Position'(x=>x, y=>y));
  end dragon_pixel;

  function dragon_start(x, y  : in Natural) return Boolean is
  begin
    return
    dragon_pixel(x+0, y+1) and then
    dragon_pixel(x+1, y+2) and then
    dragon_pixel(x+4, y+2) and then
    dragon_pixel(x+5, y+1) and then
    dragon_pixel(x+6, y+1) and then
    dragon_pixel(x+7, y+2) and then
    dragon_pixel(x+10, y+2) and then
    dragon_pixel(x+11, y+1) and then
    dragon_pixel(x+12, y+1) and then
    dragon_pixel(x+13, y+2) and then
    dragon_pixel(x+16, y+2) and then
    dragon_pixel(x+17, y+1) and then
    dragon_pixel(x+18, y+0) and then
    dragon_pixel(x+18, y+1) and then
    dragon_pixel(x+19, y+1);
  end dragon_start;

  function count_dragon_pixels return Natural is
    max : constant Natural := Natural(Ada.Numerics.Elementary_Functions.sqrt(Float(tiles.length)) * 8.0) - 1;
    cnt : Natural := 0;
  begin
    for rot in 0..3 loop
      for y in 0..max loop
        for x in 0..max loop
          if dragon_start(x, y) then
            cnt := cnt + 1;
          end if;
        end loop;
      end loop;
      TIO.put_line("Found dragons: " & cnt'IMAGE);
      if cnt /= 0 then
        return cnt * 15;
      end if;
      TIO.put_line("Rotating...");
      rotate_complete_image;
    end loop;
    TIO.put_line("Flipping...");
    flip_complete_image;
    return count_dragon_pixels;
  end count_dragon_pixels;

  procedure layout is
    type Candidate is record
      id : Natural;
      orientation : Configuration;
      left, right, up, down : Natural;
    end record;

    package Natural_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
      Element_Type => Natural);
    use Natural_Vectors;

    package Candidate_Maps is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type       => Natural,
      Element_Type    => Candidate,
      Hash            => id_hash,
      Equivalent_Keys => "=");
    use Candidate_Maps;

    function update_neighbor_links(id : in Natural; state : in out Candidate_Maps.Map) return Boolean is
      c : Candidate := state(id);
      es : constant Edge_Array := edges(c.id, c.orientation);
      t : constant Tile := tiles(c.id);
    begin
      for n of t.neighbors loop
        if state.contains(n) then
          declare
            neigh_cand : Candidate := state(n);
            neigh_es : constant Edge_Array := edges(neigh_cand.id, neigh_cand.orientation);
          begin
            if es(0) = neigh_es(2) then
              c.up := neigh_cand.id;
              neigh_cand.down := id;
            elsif es(1) = neigh_es(3) then
              c.right := neigh_cand.id;
              neigh_cand.left := id;
            elsif es(2) = neigh_es(0) then
              c.down := neigh_cand.id;
              neigh_cand.up := id;
            elsif es(3) = neigh_es(1) then
              c.left := neigh_cand.id;
              neigh_cand.right := id;
            else
              return false;
            end if;

            state(c.id) := c;
            state(neigh_cand.id) := neigh_cand;
          end;
        end if;
      end loop;
      return true;
    end update_neighbor_links;

    function layout_recurse(v : in Natural_Vectors.Vector; state : in out Candidate_Maps.Map) return Boolean is
    begin
      if v.length = 0 then
        return true;
      end if;

      declare
        t : constant Tile := tiles(v.first_element);
        rest : Natural_Vectors.Vector := v;
        c : Candidate;
      begin
        c := Candidate'(id => t.id, orientation => r1, others => 0);
        state.insert(t.id, Candidate'(id => t.id, orientation => r1, others => 0));

        rest.delete_first;
        for n of t.neighbors loop
          if not (rest.contains(n) or state.contains(n)) then
            rest.append(n);
          end if;
        end loop;

        for o in r1..fr4 loop
          declare
            new_state : Candidate_Maps.Map := state;
          begin
            c.orientation := o;
            new_state(c.id) := c;
            if update_neighbor_links(c.id, new_state) then
              if layout_recurse(rest, new_state) then
                state := new_state;
                return true;
              end if;
            end if;
          end;
        end loop;
      end;
      return false;
    end layout_recurse;

    candidates : Candidate_Maps.Map := Candidate_Maps.Empty_Map;
    vec : Natural_Vectors.Vector := Empty_Vector;
  begin
    for t of tiles loop
      if t.neighbors.length = 2 then
        vec.append(t.id);
        exit;
      end if;
    end loop;

    if layout_recurse(vec, candidates) then
      declare
        start_row : Natural;
        curr : Natural;
        x, y : Natural;
      begin
        for c of candidates loop
          if c.up = 0 and c.left = 0 then
            start_row := c.id;
            exit;
          end if;
        end loop;

        y := 0;
        while start_row /= 0 loop
          curr := start_row;
          x := 0;
          while curr /= 0 loop
            -- TIO.put(curr'IMAGE);
            update_complete_image(transform(tiles(curr).image, candidates(curr).orientation), x, y);
            curr := candidates(curr).right;
            x := x + 1;
          end loop;
          -- TIO.new_line;
          start_row := candidates(start_row).down;
          y := y + 1;
        end loop;

        put_line(complete_image);
      end;
    else
      TIO.put_line("Failed to find layout");
    end if;
  end layout;

  function image_checksum(filename : in String) return Long_Integer is
    prod : Long_Integer := 1;
  begin
    load_tiles(filename);
    match;

    for t of tiles loop
      if t.neighbors.length = 2 then
        prod := prod * Long_Integer(t.id);
      end if;
    end loop;

    return prod;
  end image_checksum;

  function water_roughness return Long_Integer is
    dragon_pixels : Natural;
  begin
    layout;
    dragon_pixels := count_dragon_pixels;
    return Long_Integer(Natural(complete_image.length) - dragon_pixels);
  end water_roughness;
end Day;
