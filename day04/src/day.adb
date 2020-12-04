-- AoC 2020, Day 4
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Day is
  package TIO renames Ada.Text_IO;

  type Key_Array is array(1..7) of String(1..3);
  required_keys : constant Key_Array := Key_Array'("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid");

  type Eye_Color_Array is array(1..7) of String(1..3);
  eye_colors : constant Eye_Color_Array := Eye_Color_Array'("amb", "blu", "brn", "gry", "grn", "hzl", "oth");

  function valid_byr(s : in String) return Boolean is
    value : constant Natural := Natural'Value(s);
  begin
    return value >= 1920 and value <= 2002;
  end valid_byr;

  function valid_iyr(s : in String) return Boolean is
    value : constant Natural := Natural'Value(s);
  begin
    return value >= 2010 and value <= 2020;
  end valid_iyr;

  function valid_eyr(s : in String) return Boolean is
    value : constant Natural := Natural'Value(s);
  begin
    return value >= 2020 and value <= 2030;
  end valid_eyr;

  function valid_hgt(s : in String) return Boolean is
    suffix : constant String := s(s'last-1..s'last);
  begin
    if suffix = "in" then
      declare
        num : constant String := s(s'first..s'last-2);
        value : constant Natural := Natural'Value(num);
      begin
        return value >= 59 and value <= 76;
      end;
    elsif suffix = "cm" then
      declare
        num : constant String := s(s'first..s'last-2);
        value : constant Natural := Natural'Value(num);
      begin
        return value >= 150 and value <= 193;
      end;
    else 
      return false;
    end if;
  end valid_hgt;

  function valid_hcl(s : in String) return Boolean is
    prefix : constant Character := s(s'first);
  begin
    if prefix /= '#' then
      return false;
    end if;

    if s'length /= 7 then
      return false;
    end if;

    for c of s(s'first+1..s'last) loop
      if not is_hexadecimal_digit(c) then
        return false;
      end if;
    end loop;

    return true;
  end valid_hcl;

  function valid_ecl(s : in String) return Boolean is
  begin
    for ec of eye_colors loop
      if s = ec then
        return true;
      end if;
    end loop;
    return false;
  end valid_ecl;

  function valid_pid(s : in String) return Boolean is
  begin
    if s'length /= 9 then
      return false;
    end if;

    for c of s loop
      if not is_digit(c) then
        return false;
      end if;
    end loop;

    return true;
  end valid_pid;

  function valid(map : in Passport_Maps.Map) return Boolean is
    use Passport_Maps;
  begin
    for elt in map.Iterate loop
      declare
        k : constant String := Key(elt);
        v : constant String := map(k);
      begin
        if k = "byr" then
          if not valid_byr(v) then
            return false;
          end if;
        elsif k = "iyr" then
          if not valid_iyr(v) then
            return false;
          end if;
        elsif k = "eyr" then
          if not valid_eyr(v) then
            return false;
          end if;
        elsif k = "hgt" then
          if not valid_hgt(v) then
            return false;
          end if;
        elsif k = "hcl" then
          if not valid_hcl(v) then
            return false;
          end if;
        elsif k = "ecl" then
          if not valid_ecl(v) then
            return false;
          end if;
        elsif k = "pid" then
          if not valid_pid(v) then
            return false;
          end if;
        end if;
      end;
    end loop;
    return true;
  end valid;

  function present(map : in Passport_Maps.Map) return Boolean is
  begin
    for k of required_keys loop
      if not map.contains(k) then
        return false;
      end if;
    end loop;
    return true;
  end present;

  function present_count(batch : in Passport_Vectors.Vector) return Count_Type is
    cnt : Natural := 0;
  begin
    for m of batch loop
      if present(m) then
        cnt := cnt + 1;
      end if;
    end loop;
    return Count_Type(cnt);
  end present_count;

  function valid_count(batch : in Passport_Vectors.Vector) return Count_Type is
    cnt : Natural := 0;
  begin
    for m of batch loop
      if present(m) and valid(m) then
        cnt := cnt + 1;
      end if;
    end loop;
    return Count_Type(cnt);
  end valid_count;

  procedure parse_entry(line : in String; map : in out Passport_Maps.Map) is
    idx : constant Natural := index(line, ":");
    left : constant String := line(line'first .. idx-1);
    right : constant String := line(idx+1 .. line'last);
  begin
    map.include(left, right);
  end parse_entry;

  procedure parse_line(line : in String; map : in out Passport_Maps.Map) is
    idx : constant Natural := index(line, " ");
  begin
    if idx > 0 then
      declare
        left : constant String := line(line'first .. idx-1);
        right : constant String := line(idx+1 .. line'last);
      begin
        parse_entry(left, map);
        parse_line(right, map);
      end;
    else
      parse_entry(line, map);
    end if;
  end parse_line;

  function "="(Left, Right:Passport_Maps.Map) return Boolean
    renames Passport_Maps."=";

  function load_batch(filename : in String) return Passport_Vectors.Vector is
    file : TIO.File_Type;
    output : Passport_Vectors.Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      declare
        map : Passport_Maps.Map;
      begin
        loop
          declare
            line : constant String := TIO.get_line(file);
          begin
            if index_non_blank(line) = 0 then
              exit;
            else
              parse_line(line, map);
              if TIO.end_of_file(file) then
                exit;
              end if;
            end if;
          end;
        end loop;
        output.append(map);
      end;
    end loop;
    TIO.close(file);
    return output;
  end load_batch;

end Day;
