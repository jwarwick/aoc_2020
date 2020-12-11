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

  function get_location(x, y : in Integer; f : Ferry) return Seat is
    loc : constant Location := Location'(X=>x, Y=>y);
  begin
    if x<0 or y<0 then
      return unocc;
    end if;
    if x>=f.width or y>=f.height then
      return unocc;
    end if;
    if contains(f.seats, loc) then
      return f.seats(loc);
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
        TIO.put(image(get_location(x, y, f)));
      end loop;
      TIO.new_line;
    end loop;
  end put_line;

  function occupied_neighbors(l : in Location; f : in Ferry) return Natural is
    occ_cnt : Natural := 0;
  begin
    for y in -1..1 loop
      for x in -1..1 loop
        if not(x=0 and y=0) then
          if occ = get_location(l.x + x, l.y + y, f) then
            occ_cnt := occ_cnt + 1;
          end if;
        end if;
      end loop;
    end loop;
    return occ_cnt;
  end occupied_neighbors;

  procedure move_up(l : in out Location) is
  begin
    l.y := l.y - 1;
  end move_up;

  procedure move_down(l : in out Location) is
  begin
    l.y := l.y + 1;
  end move_down;

  procedure move_left(l : in out Location) is
  begin
    l.x := l.x - 1;
  end move_left;

  procedure move_right(l : in out Location) is
  begin
    l.x := l.x + 1;
  end move_right;

  function tolerant_occupied_neighbors(l : in Location; f : in Ferry) return Natural is
    occ_cnt : Natural := 0;
    curr : Location := l;
    state : Seat;
  begin
    loop
      move_up(curr);
      state := get_location(curr.x, curr.y, f);
      if occ = state then
        occ_cnt := occ_cnt + 1;
        exit;
      elsif unocc = state then
        exit;
      end if;
    end loop;

    curr := l;
    loop
      move_down(curr);
      state := get_location(curr.x, curr.y, f);
      if occ = state then
        occ_cnt := occ_cnt + 1;
        exit;
      elsif unocc = state then
        exit;
      end if;
    end loop;

    curr := l;
    loop
      move_left(curr);
      state := get_location(curr.x, curr.y, f);
      if occ = state then
        occ_cnt := occ_cnt + 1;
        exit;
      elsif unocc = state then
        exit;
      end if;
    end loop;

    curr := l;
    loop
      move_right(curr);
      state := get_location(curr.x, curr.y, f);
      if occ = state then
        occ_cnt := occ_cnt + 1;
        exit;
      elsif unocc = state then
        exit;
      end if;
    end loop;

    curr := l;
    loop
      move_right(curr);
      move_up(curr);
      state := get_location(curr.x, curr.y, f);
      if occ = state then
        occ_cnt := occ_cnt + 1;
        exit;
      elsif unocc = state then
        exit;
      end if;
    end loop;

    curr := l;
    loop
      move_right(curr);
      move_down(curr);
      state := get_location(curr.x, curr.y, f);
      if occ = state then
        occ_cnt := occ_cnt + 1;
        exit;
      elsif unocc = state then
        exit;
      end if;
    end loop;

    curr := l;
    loop
      move_left(curr);
      move_down(curr);
      state := get_location(curr.x, curr.y, f);
      if occ = state then
        occ_cnt := occ_cnt + 1;
        exit;
      elsif unocc = state then
        exit;
      end if;
    end loop;

    curr := l;
    loop
      move_left(curr);
      move_up(curr);
      state := get_location(curr.x, curr.y, f);
      if occ = state then
        occ_cnt := occ_cnt + 1;
        exit;
      elsif unocc = state then
        exit;
      end if;
    end loop;

    return occ_cnt;
  end tolerant_occupied_neighbors;

  procedure step(f : in out Ferry) is
    old_ferry : constant Ferry := f;
    new_seats : Seat_Map.Map := f.seats;
    neigh : Natural := 0;
  begin
    for c in old_ferry.seats.iterate loop
      neigh := occupied_neighbors(key(c), old_ferry);
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

  procedure tolerant_step(f : in out Ferry) is
    old_ferry : constant Ferry := f;
    new_seats : Seat_Map.Map := f.seats;
    neigh : Natural := 0;
  begin
    for c in old_ferry.seats.iterate loop
      neigh := tolerant_occupied_neighbors(key(c), old_ferry);
      case element(c) is
        when unocc =>
          if 0 = neigh then
            new_seats(key(c)) := occ;
          end if;
        when occ =>
          if neigh >= 5 then
            new_seats(key(c)) := unocc;
          end if;
        when floor => null;
      end case;
    end loop;
    f.seats := new_seats;
  end tolerant_step;

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

  function tolerant_steady_state_occupied(m : in Ferry) return Natural is
    f : Ferry := m;
    last_seats : Seat_Map.Map := f.seats;
  begin
    loop
      tolerant_step(f);
      if last_seats = f.seats then
        exit;
      end if;
      last_seats := f.seats;
    end loop;
    return count_occupied(f.seats);
  end tolerant_steady_state_occupied;
end Day;
