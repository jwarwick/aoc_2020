-- AOC 2020, Day 12
with Ada.Containers.Vectors;

package Day is
  type Ferry is private;
  Instruction_Exception : exception;

  function load_file(filename : in String) return Ferry;
  function distance(f : in Ferry) return Natural;

  private
  type Action_Type is (north, south, east, west, left, right, forward);
  type Heading_Type is (north, east, south, west);

  type Instruction is record
    action : Action_Type := north;
    value : Integer := 0;
  end record;

  package Instruction_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Instruction);
  use Instruction_Vectors;

  type Ferry is record
    instructions : Instruction_Vectors.Vector := Empty_Vector;
    heading : Heading_Type := east;
    x : Integer := 0;
    y : Integer := 0;
  end record;
end Day;
