-- AoC 2020, Day 2
with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

package body Day is
  package TIO renames Ada.Text_IO;

  pragma Warnings (Off, "procedure ""put"" is not referenced");
  procedure put(value : in Password_Entry) is
  pragma Warnings (On, "procedure ""put"" is not referenced");
  begin
    TIO.Put_Line(Natural'Image(value.min) & "-" & Natural'Image(value.max) & " " & to_string(value.pattern) & ": " & to_string(value.password));
  end put;

  pragma Warnings (Off, "procedure ""put"" is not referenced");
  procedure put(value : in Password_Vector.Vector) is
  pragma Warnings (On, "procedure ""put"" is not referenced");
  begin
    for v of value loop
      put(v);
    end loop;
  end put;

  procedure parse_line(line : in String; vec : in out Password_Vector.Vector) is
    hyphen : constant Natural := index(line, "-");
    space : constant Natural := index(line, " ");
    min : constant String := line(line'first .. hyphen-1);
    max : constant String := line(hyphen+1 .. space-1);
    pattern : constant String := line(space+1 .. space+1);
    str : constant String := line(space+4 .. line'last);
    curr : constant Password_Entry :=
      Password_Entry'(Min => Natural'VALUE(min), Max => Natural'VALUE(max), Pattern => to_unbounded_string(pattern), Password => to_unbounded_string(str));
  begin 
    vec.append(curr);
  end parse_line;

  function load_passwords(filename : in String) return Password_Vector.Vector is
    file : TIO.File_Type;
    vec : Password_Vector.Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      parse_line(TIO.get_line(file), vec);
    end loop;
    TIO.close(file);
    return vec;
  end load_passwords;

  function count_valid(passwords : in Password_Vector.Vector) return Ada.Containers.Count_Type is
    valid : Natural := 0;
  begin
    for e of passwords loop
      declare
        cnt : constant Natural := Count(Source => e.Password, Pattern => to_string(e.Pattern));
      begin
        if cnt in e.Min .. e.Max then
          valid := valid + 1;
        end if;
      end;
    end loop;
    return Ada.Containers.Count_Type(valid);
  end count_valid;
end Day;
