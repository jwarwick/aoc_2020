-- AoC 2020, Day 13
with Ada.Text_IO;
with GNAT.String_Split;
use GNAT;

package body Day is
  package TIO renames Ada.Text_IO;

  function load_file(filename : in String) return Schedule is
    file : TIO.File_Type;
    earliest : Natural;
    departs : Depart_Vectors.Vector := Empty_Vector;
    subs : String_Split.Slice_Set;
    seps : constant String := ",";
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    earliest := Natural'Value(TIO.get_line(file));
    String_Split.Create (S => subs, From => TIO.get_line(file), Separators => seps, Mode => String_Split.Multiple);
    TIO.close(file);
    for i in 1 .. String_Split.Slice_Count(subs) loop
      declare
        sub : constant String := String_Split.Slice(subs, i);
      begin
        if sub /= "x" then
          departs.append(Natural'Value(sub));
        end if;
        -- TIO.Put_Line (String_Split.Slice_Number'Image (I) &
        -- " -> " &
        -- Sub &
        -- " (length" & Positive'Image (Sub'Length) &
        -- ")");
      end;
    end loop;
    return Schedule'(earliest => earliest, departures => departs);
  end load_file;

  function bus_mult(s : in Schedule) return Natural is
    offset : Natural := 0;
  begin
    TIO.put_line("earliest: " & Natural'Image(s.earliest));
    for d of s.departures loop
      TIO.put_line(Natural'Image(d));
    end loop;

    loop
      declare
        leave : constant Natural := s.earliest + offset;
      begin
        for d of s.departures loop
          if (leave mod d) = 0 then
            return offset * d;
          end if;
        end loop;
        offset := offset + 1;
      end;
    end loop;
  end bus_mult;
end Day;
