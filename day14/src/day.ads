-- AOC 2020, Day 1$
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Ordered_Maps;

package Day is
  type Program is private;

  function load_file(filename : in String) return Program;
  function sum_memory(p : in Program) return Long_Integer;
  function sum_memory_v2(p : in Program) return Long_Integer;

  private
  type Instruction_Type is (mask, mem);

  type Mask_Field is (zero, one, floating, hard_zero);

  package Mask_Ordered_Maps is new Ada.Containers.Indefinite_Ordered_Maps
    (Key_Type        => Natural,
    Element_Type    => Mask_Field);
   use Mask_Ordered_Maps;

  type Instruction(Kind : Instruction_Type := mem) is record
    case Kind is
      when mem => 
        offset : Long_Integer := 0;
        value : Long_Integer := 0;
      when mask =>
        mask_map : Mask_Ordered_Maps.Map := Mask_Ordered_Maps.Empty_Map;
    end case;
  end record;

  package Instruction_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Instruction);
  use Instruction_Vectors;

  package Memory_Ordered_Maps is new Ada.Containers.Indefinite_Ordered_Maps
    (Key_Type        => Long_Integer,
    Element_Type    => Long_Integer);
   use Memory_Ordered_Maps;

  type Program is record
    instructions : Instruction_Vectors.Vector := Empty_Vector;
    memory : Memory_Ordered_Maps.Map := Memory_Ordered_Maps.Empty_Map;
    curr_mask : Mask_Ordered_Maps.Map := Mask_Ordered_Maps.Empty_Map;
  end record;
end Day;
