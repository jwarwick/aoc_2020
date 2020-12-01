-- AOC 2020, Day 1
with Ada.Containers.Vectors;

package Day is
  type Expense is new Natural;

  function part1(filename : in String) return Expense;

  private
  package Expense_Vector is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Expense);

  function load_file(filename : in String) return Expense_Vector.Vector;
  function matching_product(expenses : in Expense_Vector.Vector) return Expense;
end Day;
