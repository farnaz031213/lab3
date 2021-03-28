
---------------------------------------------------------------------------
-- adder_16b.vhd - 16-bit Adder Implementation
--
--
-- Copyright (C) 2006 by Lih Wen Koh (lwkoh@cse.unsw.edu.au)
-- All Rights Reserved. 
--
-- The single-cycle processor core is provided AS IS, with no warranty of 
-- any kind, express or implied. The user of the program accepts full 
-- responsibility for the application of the program and the use of any 
-- results. This work may be downloaded, compiled, executed, copied, and 
-- modified solely for nonprofit, educational, noncommercial research, and 
-- noncommercial scholarship purposes provided that this notice in its 
-- entirety accompanies all copies. Copies of the modified software can be 
-- delivered to persons who use it solely for nonprofit, educational, 
-- noncommercial research, and noncommercial scholarship purposes provided 
-- that this notice in its entirety accompanies all copies.
--
---------------------------------------------------------------------------


-- should be changed to add sll 
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
--use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
USE ieee.numeric_std.ALL;


entity adder_16b is
--    port ( ALUOperation: in std_logic_vector(3 downto 0);
--           src_a     : in  std_logic_vector(15 downto 0);
--           src_b     : in  std_logic_vector(15 downto 0);
--           result      : out std_logic_vector(15 downto 0);
--           carry_out : out std_logic );
     port ( 
            clk     : in std_logic;
            ID_EX   : in std_logic_vector(60 downto 0);
            EX_MEM  : out std_logic_vector(18 downto 0) );
end adder_16b;

architecture behavioural of adder_16b is
signal op:   std_logic_vector(3 downto 0);
signal src_a : std_logic_vector(15 downto 0);
signal src_b: std_logic_vector(15 downto 0);
signal sig_result : std_logic_vector(16 downto 0);

begin
   
    
    read_write: process(clk)
    --operation: process(ALUOperation,src_a,src_b)
      
        begin
        
        --on the rising edge reading the value from ID_EX
        if (rising_edge(clk)) then 
             op <= ID_EX( 55 downto 52);
             src_a <= ID_EX( 47 downto 32);
             src_b <= ID_EX( 31 downto 16);
        end if; 
        -- on the falling edge writting the result value and passing come control registers to ex_mem   
        if (falling_edge(clk)) then
            
            -- writing the result of the operation to the register
            EX_MEM(15 downto 0) <= sig_result(15 downto 0);
            
            -- passing reg write from ID_EX to EX_MEM 
            EX_MEM(18) <= ID_EX(59);
            
            -- passing mem_write from ID_EX to EX_MEM
            EX_MEM(17) <= ID_EX(57);
            
            -- passing mem_to_red fom ID_EX to EX_MEM
            EX_MEM(16) <= ID_EX(56); 
            
        end if;
        end process;
     
     
     -- this process will do the operation    
     operation: process(op, src_a, src_b)
        variable convert: unsigned(15 downto 0);
        variable shiftVal: Integer range 0 to 15;
        
     begin
        case op is 
            -- shift
                
            when "0100" =>
                convert := unsigned(src_a);
                shiftVal := TO_INTEGER(unsigned(src_b));
                sig_result(15 downto 0) <= std_logic_vector(convert sll shiftVal);
                --EX_MEM(15 downto 0) <= sig_result(15 downto 0);
                --result <= src_a;
                --result <= "1111111111111111";
             
            -- addition
            when "1000" | "0001" =>
                sig_result <= ('0' & src_a) + ('0' & src_b);
                --EX_MEM(15 downto 0)     <= sig_result(15 downto 0);
                --carry_out  <= sig_result(16);
           
           -- subtraction for bne
           when "0101" =>
                    sig_result <= ('0' & src_a) - ('0' & src_b);
                    --EX_MEM(15 downto 0)     <= sig_result(15 downto 0);
                    --carry_out  <= sig_result(16);
--                result(15 downto 12) <= ALUOperation;
--                result(11 downto 0) <= "111111111111";
            when others =>
            
        end case;
        end process;
    
end behavioural;
