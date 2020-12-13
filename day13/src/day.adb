-- AoC 2020, Day 13
with Ada.Text_IO;
with GNAT.String_Split;
use GNAT;

package body Day is
  package TIO renames Ada.Text_IO;

  function load_file(filename : in String) return Schedule is
    file : TIO.File_Type;
    earliest : Long_Long_Integer;
    departs : Depart_Vectors.Vector := Empty_Vector;
    offsets : Depart_Vectors.Vector := Empty_Vector;
    subs : String_Split.Slice_Set;
    seps : constant String := ",";
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    earliest := Long_Long_Integer'Value(TIO.get_line(file));
    String_Split.Create (S => subs, From => TIO.get_line(file), Separators => seps, Mode => String_Split.Multiple);
    TIO.close(file);
    for i in 1 .. String_Split.Slice_Count(subs) loop
      declare
        sub : constant String := String_Split.Slice(subs, i);
      begin
        if sub /= "x" then
          departs.append(Long_Long_Integer'Value(sub));
          offsets.append(Long_Long_Integer(i) - 1);
        end if;
      end;
    end loop;
    return Schedule'(earliest => earliest, departures => departs, offsets => offsets);
  end load_file;

  function bus_mult(s : in Schedule) return Long_Long_Integer is
    offset : Long_Long_Integer := 0;
  begin
    loop
      declare
        leave : constant Long_Long_Integer := s.earliest + offset;
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

  function earliest_matching_iterative(s : in Schedule) return Long_Long_Integer is
    t : Long_Long_Integer := 1;
  begin
    main_loop:
    loop
      declare
        match : Boolean := true;
        base : constant Long_Long_Integer := Long_Long_Integer(s.departures.first_element) * t;
      begin
        for i in s.departures.first_index+1 .. s.departures.last_index loop
          declare
            long_offset : constant Long_Long_Integer := s.offsets(i);
            long_depart : constant Long_Long_Integer := s.departures(i);
            offset : constant Long_Long_Integer := base + long_offset;
          begin
          if (offset mod long_depart) /= 0 then
            match := false;
            exit;
          end if;
        end;
        end loop;

        if match then
          return base;
        end if;
        t := t+1;
      end;
    end loop main_loop;
  end earliest_matching_iterative;

  function bus_intersect(start : in Long_Long_Integer; step : in Long_Long_Integer;
    bus : in Long_Long_Integer; offset : in Long_Long_Integer) return Long_Long_Integer is
    t : Long_Long_Integer := start;
    offset_t : Long_Long_Integer;
  begin
    loop
      offset_t := t + offset;
      if offset_t mod bus = 0 then
        exit;
      end if;
      t := t + step;
    end loop;
    return t;
  end bus_intersect;

  function earliest_matching(s : in Schedule) return Long_Long_Integer is
    t : Long_Long_Integer := 0;
    step : Long_Long_Integer := s.departures.first_element;
  begin
    for i in s.departures.first_index+1 .. s.departures.last_index loop
      t := bus_intersect(t, step, s.departures(i), s.offsets(i));
      step := step * s.departures(i);
    end loop;
    return t;
  end earliest_matching;

end Day;
