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
    for i of expenses loop
      for j of expenses loop
        if i + j = 2020 then
          return i * j;
        end if;
      end loop;
    end loop;
    return 0;
  end matching_product;

  -- Find two entries that sum to 2020 and return their product
  function part1(filename : in String) return Expense is
    expenses : Expense_Vector.Vector;
  begin
    expenses := load_file(filename);
    return matching_product(expenses);
  end part1;
end Day;
