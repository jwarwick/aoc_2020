-- AoC 2020, Day 1
with Ada.Text_IO;

package body Day is
  package TIO renames Ada.Text_IO;

  function load_file(filename : in String) return Expense_Vector.Vector is
    file : TIO.File_Type;
    expenses : Expense_Vector.Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      expenses.append(Expense'Value(TIO.get_line(file)));
    end loop;
    TIO.close(file);
    return expenses;
  end load_file;

  function matching_product(expenses : in Expense_Vector.Vector) return Expense is
  begin
    i_loop:
    for i of expenses loop
      j_loop:
      for j of expenses loop
        if i + j = 2020 then
          return i * j;
        end if;
      end loop j_loop;
    end loop i_loop;
    return 0;
  end matching_product;

  function triple_matching_product(expenses : in Expense_Vector.Vector) return Expense is
  begin
    i_loop:
    for i of expenses loop
      j_loop:
      for j of expenses loop
        if i + j < 2020 then
          k_loop:
          for k of expenses loop
            if i + j + k = 2020 then
              return i * j * k;
            end if;
          end loop k_loop;
        end if;
      end loop j_loop;
    end loop i_loop;
    return 0;
  end triple_matching_product;

  -- Find two entries that sum to 2020 and return their product
  function part1(filename : in String) return Expense is
    expenses : Expense_Vector.Vector;
  begin
    expenses := load_file(filename);
    return matching_product(expenses);
  end part1;

  -- Find three entries that sum to 2020 and return their product
  function part2(filename : in String) return Expense is
    expenses : Expense_Vector.Vector;
  begin
    expenses := load_file(filename);
    return triple_matching_product(expenses);
  end part2;
end Day;
