-- AoC 2020, Day 11
with Ada.Text_IO;

package body Day is
  package TIO renames Ada.Text_IO;

  function "<"(left, right : in Location) return Boolean is
  begin
    if left.y < right.y then
      return true;
    elsif left.y = right.y and left.x < right.x then
      return true;
    else
      return false;
    end if;
  end "<";

  function parse_line(line : in String; y : in Natural; m : in out Seat_Map.Map) return Natural is
    x : Natural := 0;
  begin
    for c of line loop
      if c = 'L' then
        m.insert(Location'(X => x, Y => y), unocc);
      elsif c = '#' then
        m.insert(Location'(X => x, Y => y), occ);
      end if;
      x := x + 1;
    end loop;
    return x;
  end parse_line;

  function load_file(filename : in String) return Ferry is
    file : TIO.File_Type;
    m : Seat_Map.Map := Empty_Map;
    y : Natural := 0;
    width : Natural := 0;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      width := parse_line(TIO.get_line(file), y, m);
      y := y + 1;
    end loop;
    TIO.close(file);
    return Ferry'(seats => m, width => width, height => y);
  end load_file;

  function get_location(x, y : in Integer; m : Seat_Map.Map) return Seat is
    loc : constant Location := Location'(X=>x, Y=>y);
  begin
    if contains(m, loc) then
      return m(loc);
    else
      return floor;
    end if;
  end get_location;

  function image(l : in Seat) return Character is
  begin
    case l is
      when occ => return '#';
      when unocc => return 'L';
      when floor => return '.';
    end case;
  end image;

  pragma Warnings (Off);
  procedure put_line(f : in Ferry) is
  pragma Warnings (On);
  begin
    TIO.put_line(Natural'Image(f.width) & "x" & Natural'Image(f.height));
    for y in 0..f.height-1 loop
      for x in 0..f.width-1 loop
        TIO.put(image(get_location(x, y, f.seats)));
      end loop;
      TIO.new_line;
    end loop;
  end put_line;

  function occupied_neighbors(l : in Location; m : in Seat_Map.Map) return Natural is
    occ_cnt : Natural := 0;
  begin
    for y in -1..1 loop
      for x in -1..1 loop
        if not(x=0 and y=0) then
          if occ = get_location(l.x + x, l.y + y, m) then
            occ_cnt := occ_cnt + 1;
          end if;
        end if;
      end loop;
    end loop;
    return occ_cnt;
  end occupied_neighbors;

  procedure step(f : in out Ferry) is
    old_seats : constant Seat_Map.Map := f.seats;
    new_seats : Seat_Map.Map := f.seats;
    neigh : Natural := 0;
  begin
    for c in old_seats.iterate loop
      neigh := occupied_neighbors(key(c), old_seats);
      case element(c) is
        when unocc =>
          if 0 = neigh then
            new_seats(key(c)) := occ;
          end if;
        when occ =>
          if neigh >= 4 then
            new_seats(key(c)) := unocc;
          end if;
        when floor => null;
      end case;
    end loop;
    f.seats := new_seats;
  end step;

  function count_occupied(m : in Seat_Map.Map) return Natural is
    cnt : Natural := 0;
  begin
    for c in m.iterate loop
      if occ = element(c) then
        cnt := cnt + 1;
      end if;
    end loop;
    return cnt;
  end count_occupied;

  function steady_state_occupied(m : in Ferry) return Natural is
    f : Ferry := m;
    last_seats : Seat_Map.Map := f.seats;
  begin
    loop
      step(f);
      if last_seats = f.seats then
        exit;
      end if;
      last_seats := f.seats;
    end loop;
    return count_occupied(f.seats);
  end steady_state_occupied;
end Day;
