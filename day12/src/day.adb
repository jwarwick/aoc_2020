-- AoC 2020, Day 12
with Ada.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;

package body Day is
  package TIO renames Ada.Text_IO;

  function char_to_action(c : in Character) return Action_Type is
  begin
    case c is
      when 'N' => return north;
      when 'S' => return south;
      when 'E' => return east;
      when 'W' => return west;
      when 'L' => return left;
      when 'R' => return right;
      when 'F' => return forward;
      when others => raise Instruction_Exception with "Unknown action: " & c;
    end case;
  end;

  function parse_line(line : in String) return Instruction is
    act : constant Action_Type := char_to_action(line(line'first));
    val : constant Integer := Integer'Value(line(line'first+1..line'last));
  begin
    return Instruction'(action => act, value => val);
  end parse_line;

  function load_file(filename : in String) return Ferry is
    file : TIO.File_Type;
    vec : Instruction_Vectors.Vector := Empty_Vector;
  begin
    TIO.open(File => file, Mode => TIO.In_File, Name => filename);
    while not TIO.end_of_file(file) loop
      vec.append(parse_line(TIO.get_line(file)));
    end loop;
    TIO.close(file);
    return Ferry'(instructions => vec, heading => east, x => 0, y => 0, way_x => -1, way_y => 10);
  end load_file;

  function action_to_heading(act : in Action_Type) return Heading_Type is
  begin
    case act is
      when north => return north;
      when south => return south;
      when east => return east;
      when west => return west;
      when others => raise Instruction_Exception with "Cannot map action to heading: " & Action_Type'Image(act);
    end case;
  end action_to_heading;

  procedure move(dir : in Heading_Type; dist : in Integer; f : in out Ferry) is
  begin
    case dir is
      when north => f.x := f.x - dist;
      when south => f.x := f.x + dist;
      when east => f.y := f.y + dist;
      when west => f.y := f.y - dist;
    end case;
  end move;

  procedure turn(angle : in Integer; f : in out Ferry) is
    curr_pos : constant Natural := Heading_Type'Pos(f.heading);
    steps : constant Integer := angle / 90;
    new_pos : constant Natural := (curr_pos + steps) mod 4;
    new_heading : constant Heading_Type := Heading_Type'Val(new_pos);
  begin
    f.heading := new_heading;
  end turn;

  procedure simulate(f : in out Ferry) is
  begin
    for i of f.instructions loop
      case i.action is
        when north..west => move(action_to_heading(i.action), i.value, f);
        when forward => move(f.heading, i.value, f);
        when left => turn(-1 * i.value, f);
        when right => turn(i.value, f);
      end case;
    end loop;
  end simulate;

  procedure move_waypoint(dir : in Heading_Type; dist : in Integer; f : in out Ferry) is
  begin
    case dir is
      when north => f.way_x := f.way_x - dist;
      when south => f.way_x := f.way_x + dist;
      when east => f.way_y := f.way_y + dist;
      when west => f.way_y := f.way_y - dist;
    end case;
  end move_waypoint;

  procedure rotate_waypoint(angle : in Integer; f : in out Ferry) is
    RadPerDegree : constant Float := Ada.Numerics.Pi / 180.0;
    radians : constant Float := RadPerDegree * Float(angle);
    x : constant Float := Float(f.way_x);
    y : constant Float := Float(f.way_y);
    new_x : constant Float := (x * cos(radians)) - (y * sin(radians));
    new_y : constant Float := (x * sin(radians)) + (y * cos(radians));
  begin
    f.way_x := Integer(new_x);
    f.way_y := Integer(new_y);
  end rotate_waypoint;

  procedure simulate_waypoint(f : in out Ferry) is
  begin
    for i of f.instructions loop
      case i.action is
        when north..west => move_waypoint(action_to_heading(i.action), i.value, f);
        when forward => 
          f.x := f.x + (i.value * f.way_x);
          f.y := f.y + (i.value * f.way_y);
        when left => rotate_waypoint(i.value, f);
        when right => rotate_waypoint(-1 * i.value, f);
      end case;
    end loop;
  end simulate_waypoint;

  function distance(f : in Ferry) return Natural is
    tmp : Ferry := f;
  begin
    simulate(tmp);
    return abs(tmp.x) + abs(tmp.y);
  end distance;

  function waypoint_distance(f : in Ferry) return Natural is
    tmp : Ferry := f;
  begin
    simulate_waypoint(tmp);
    return abs(tmp.x) + abs(tmp.y);
  end waypoint_distance;
end Day;
