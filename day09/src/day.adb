-- AoC 2020, Day 9
with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;

package body Day is
  package TIO renames Ada.Text_IO;

  function load_file(filename : in String) return XMAS_Vector.Vector is
    file : TIO.File_Type;
    v : XMAS_Vector.Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      v.append(Long_Integer'Value(TIO.get_line(file)));
    end loop;
    TIO.close(file);
    return v;
  end load_file;

  package Sum_Sets is new Ada.Containers.Ordered_Sets (Element_Type => Long_Integer);
  use Sum_Sets;

  package Sum_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Sum_Sets.Set);
  use Sum_Vectors;

  function initialize_sums(v : in XMAS_Vector.Vector; preamble : in Positive) return Sum_Vectors.Vector is
    sums : Sum_Vectors.Vector := Sum_Vectors.Empty_Vector;
    last_idx : constant Natural := preamble-1;
  begin
    for i in 0..preamble-1 loop
      declare
        set : Sum_Sets.Set := Empty_Set;
        final : constant Natural :=  Natural'Min(i+preamble, last_idx);
      begin
        for j in i+1..final loop
          set.include(v(i) + v(j));
        end loop;
        sums.append(set);
      end;
    end loop;
    return sums;
  end initialize_sums;

  procedure update_sums(sums : in out Sum_Vectors.Vector; v : in XMAS_Vector.Vector; index : in Positive) is
    start_index : constant Natural := index - (Natural(length(sums)) - 1);
    new_value : constant Long_Integer := v(index);
    new_set : Sum_Sets.Set := Empty_Set;
  begin
    sums.delete(0);
    for i in 0..Natural(length(sums))-1 loop
      declare
        curr_set : Sum_Sets.Set := sums(i);
      begin
        curr_set.include(v(start_index + i) + new_value);
        sums(i) := curr_set;
      end;
    end loop;
    new_set.include(new_value + v(index-1));
    sums.append(new_set);
  end update_sums;

  function contains(sums: Sum_Vectors.Vector; target : in Long_Integer) return Boolean is
  begin
    for set of sums loop
      if contains(set, target) then
        return true;
      end if;
    end loop;
    return false;
  end contains;

  function first_invalid(v : in XMAS_Vector.Vector; preamble : in Positive) return Long_Integer is
    sums : Sum_Vectors.Vector := initialize_sums(v, preamble);
  begin
    for idx in preamble..Natural(length(v))-1 loop
      if not contains(sums, v(idx)) then
        return v(idx);
      end if;
      update_sums(sums, v, idx);
    end loop;
    return 0;
  end first_invalid;

  -- package Running_Sum_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Long_Integer);
  -- use Running_Sum_Vectors;

  function min_max_sum(v : in XMAS_Vector.Vector; start_idx : in Natural; end_idx : in Natural) return Long_Integer is
    min : Long_Integer := Long_Integer'Last;
    max : Long_Integer := 0;
  begin
    for idx in start_idx..end_idx loop
      if min > v(idx) then
        min := v(idx);
      end if;
      if max < v(idx) then
        max := v(idx);
      end if;
    end loop;
    return min + max;
  end min_max_sum;

  function find_sum(v : in XMAS_Vector.Vector; target : in Long_Integer) return Long_Integer is
    -- sums : Running_Sum_Vectors.Vector := Running_Sum_Vectors.Empty_Vector;
    sum : Long_Integer := 0;
  begin
    for i in  v.first_index .. v.last_index loop
      sum := 0;
      for j in i .. v.last_index loop
        sum := sum + v(j);
        if sum = target then
          return min_max_sum(v, i, j);
        elsif sum > target then
          exit;
        end if;
      end loop;
    end loop;
    return 0;
  end find_sum;

  -- function find_sum(v : in XMAS_Vector.Vector; target : in Long_Integer) return Long_Integer is
  --   sums : Running_Sum_Vectors.Vector := Running_Sum_Vectors.Empty_Vector;
  --   last_idx : constant Natural := Natural(length(v)) - 1;
  --   start_idx : Natural := 0;
  --   end_idx : Natural := start_idx + 1;
  --   sum : Long_Integer := v(start_idx);
  --   old_start : Long_Integer := 0;
  -- begin
  --   loop
  --     sum := sum + v(end_idx);
  --     if sum = target then
  --       return min_max_sum(v, start_idx, end_idx);
  --     elsif sum < target then
  --       sums.append(sum);
  --       end_idx := end_idx + 1;
  --     else -- > target
  --       exit;
  --     end if;
  --   end loop;

  --   end_idx := end_idx - 1;
  --   for v of sums loop
  --     TIO.put(Long_Integer'Image(v) & " ");
  --   end loop;
  --   TIO.new_line;
  --   TIO.put_line(Natural'Image(start_idx) & "," & Natural'Image(end_idx));

  --   loop
  --     TIO.put_line("Getting start: " & Natural'Image(start_idx));
  --     old_start := v(start_idx);
  --     TIO.put_line("Got it");
  --     start_idx := start_idx + 1;
  --     end_idx := Natural'min(end_idx + 1, last_idx);
  --     sums.delete(0);
  --     for idx in sums.first_index .. sums.last_index loop
  --       if start_idx+idx >= last_idx then
  --         TIO.put_line("Jumping out");
  --         exit;
  --       end if;
  --       TIO.put_line("Trying " & Natural'Image(start_idx) & "-" & Natural'Image(start_idx + idx));
  --       TIO.put_line("Endidx: " & Natural'Image(end_idx));
  --       sum := sums(idx) - old_start;
  --       -- TIO.put_line("Targ: " & Long_Integer'Image(target) & ", sum: " & Long_Integer'Image(sum));
  --       -- TIO.put_line("sum: " & Natural'Image(start_idx) & "-" & Natural'Image(start_idx+idx) & "=" & Long_Integer'Image(sum));
  --       if sum = target then
  --         return min_max_sum(v, start_idx, start_idx + idx);
  --       end if;
  --       if sum > target then
  --         declare
  --           last : constant Natural := sums.last_index;
  --         begin
  --           -- TIO.put_line("idx: " & Natural'Image(idx) & ",last:" & Natural'Image(last));
  --           for di in idx..last loop
  --             -- TIO.put_line("Targ: " & Long_Integer'Image(target) & ", sum: " & Long_Integer'Image(sum));
  --             -- TIO.put_line("DELETING!!!!!!!!!!!!!!!!!!!!!");
  --             sums.delete(di);
  --           end loop;
  --         end;
  --       end if;
  --     end loop;
  --     TIO.put_line("Accessing end_idx" & Natural'Image(end_idx));
  --     sum := sum + v(end_idx);
  --     TIO.put_line("Got it");
  --     if sum = target then
  --       return min_max_sum(v, start_idx, end_idx);
  --     end if;
  --     if sum < target then
  --       TIO.put_line("Adding index" & Natural'Image(end_idx));
  --       TIO.put_line("Added " & Long_Integer'Image(sum));
  --       sums.append(sum);
  --       -- end_idx := end_idx + 1;
  --       -- sum := sum + v(end_idx);
  --     end if;
  --     -- if target > sum + v(end_idx+1) then
  --     --   TIO.put_line("need to add");
  --     --   exit;
  --     -- end if;
  --   end loop;
  --   -- return 8;
  -- end find_sum;
end Day;
