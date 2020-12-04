-- AoC 2020, Day 4
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Day is
  package TIO renames Ada.Text_IO;

  type Key_Array is array(1..7) of String(1..3);
  required_keys : constant Key_Array := Key_Array'("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid");

  function valid(map : in Passport_Maps.Map) return Boolean is
  begin
    for k of required_keys loop
      if not map.contains(k) then
        return false;
      end if;
    end loop;
    return true;
  end valid;

  function valid_count(batch : in Passport_Vectors.Vector) return Count_Type is
    cnt : Natural := 0;
  begin
    for m of batch loop
      if valid(m) then
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
