
---------------------------------------------------------------------------
-- instruction_memory.vhd - Implementation of A Single-Port, 16 x 16-bit
--                          Instruction Memory.
-- 
-- Notes: refer to headers in single_cycle_core.vhd for the supported ISA.
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
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity instruction_memory is
--    port ( reset    : in  std_logic;
--           clk      : in  std_logic;
--           addr_in  : in  std_logic_vector(3 downto 0);
--           insn_out : out std_logic_vector(15 downto 0) );
      port ( reset    : in  std_logic;
           clk      : in  std_logic;
           addr_in  : in  std_logic_vector(3 downto 0);
           IF_ID : out std_logic_vector(19 downto 0) );
end instruction_memory;

architecture behavioral of instruction_memory is

type mem_array is array(0 to 15) of std_logic_vector(15 downto 0);
signal sig_insn_mem : mem_array;

begin
    mem_process: process ( clk,
                           addr_in ) is
  
    variable var_insn_mem : mem_array;
    variable var_addr     : integer;
  
    begin
        if (reset = '1') then
            -- initial values of the instruction memory :
            --  insn_0 : push mem(0) to stack
            --  insn_1 : push mem(1) to stack
            --  insn_2 : add   sp+(sp+1)
            --  insn_4 : pop savea at mem(2)
            --  insn_6: sll sp by immediate
           
            var_insn_mem(0)  := X"1010"; --1: op 0: sources 1: destinatoin offeset: 0
            var_insn_mem(1)  := X"1021"; --1:load   source:2   destination 0 offset: 1
            var_insn_mem(2)  := X"8013";
            var_insn_mem(3)  := X"8124";
            var_insn_mem(4)  := X"3032";
            var_insn_mem(5)  := X"3043";
            var_insn_mem(6)  := X"4344";  --assigning code to sll $4,$3,$2   shift 3 by value in 2 and save in 4
            var_insn_mem(7)  := X"4123";
            var_insn_mem(8)  := X"5342";
            var_insn_mem(9)  := X"1012";
            var_insn_mem(10) := X"1023";
            var_insn_mem(11) := X"4124";
            var_insn_mem(12) := X"5113";
            var_insn_mem(13) := X"8111";
            var_insn_mem(14) := X"3010";
            var_insn_mem(15) := X"3021";
            --
        
        elsif (falling_edge(clk)) then
            -- read instructions on the rising clock edge
            var_addr := conv_integer(addr_in);
            IF_ID(15 downto 0) <= var_insn_mem(var_addr);
            IF_ID(19 downto 16) <= addr_in;
            
        end if;

        -- the following are probe signals (for simulation purpose)
        sig_insn_mem <= var_insn_mem;

    end process;
  
end behavioral;
