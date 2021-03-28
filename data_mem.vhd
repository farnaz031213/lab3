
---------------------------------------------------------------------------
-- data_memory.vhd - Implementation of A Single-Port, 16 x 16-bit Data
--                   Memory.
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
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;

entity data_memory is
--    port ( reset        : in  std_logic;
--           clk          : in  std_logic;
--           write_enable : in  std_logic;
--           write_data   : in  std_logic_vector(15 downto 0);
--           addr_in      : in  std_logic_vector(3 downto 0);
--           data_out     : out std_logic_vector(15 downto 0) );

    port ( reset        : in  std_logic;
           clk          : in  std_logic;
           EX_MEM       : in std_logic_vector(18 downto 0);
           write_enable : in  std_logic;
           write_data   : in  std_logic_vector(15 downto 0);
           addr_in      : in  std_logic_vector(3 downto 0);
           MEM_WB       : out std_logic_vector( 16 downto 0);
           data_out     : out std_logic_vector(15 downto 0) );
end data_memory;

architecture behavioral of data_memory is

type mem_array is array(0 to 15) of std_logic_vector(15 downto 0);
signal sig_data_mem : mem_array;
signal var_addr: integer;

begin
    mem_process: process ( clk,
                           write_enable,
                           write_data,
                           addr_in ) is
  
    variable var_data_mem : mem_array;
    --variable var_addr     : integer;
  
    begin
       
        
        if (reset = '1') then
            -- initial values of the data memory : reset to zero 
            var_data_mem(0)  := X"0005";
            var_data_mem(1)  := X"0008";
            var_data_mem(2)  := X"0000";
            var_data_mem(3)  := X"0000";
            var_data_mem(4)  := X"0000";
            var_data_mem(5)  := X"0000";
            var_data_mem(6)  := X"0000";
            var_data_mem(7)  := X"0000";
            var_data_mem(8)  := X"0000";
            var_data_mem(9)  := X"0000";
            var_data_mem(10) := X"0000";
            var_data_mem(11) := X"0000";
            var_data_mem(12) := X"0000";
            var_data_mem(13) := X"0000";
            var_data_mem(14) := X"0000";
            var_data_mem(15) := X"0000";

--        elsif (falling_edge(clk) and write_enable = '1') then
--            -- memory writes on the falling clock edge
--            var_data_mem(var_addr) := write_data;
--        end if;

        -- on the rising_edge we will read from EX_MEM
        else 
        
            if (rising_edge(clk)) then
                 var_addr <= conv_integer(EX_MEM(3 downto 0));
                 
            end if;
            
            -- on falling edge we will do the operation and write the result back to the pipelien register
            -- I am just passing along the address
            -- I am not passing along what has to be written 
            -- I need to include that in the EX_MEM so that It could be read  
            if (falling_edge(clk)) then
            
                if (EX_MEM(17) = '1') then
                    var_data_mem(var_addr) := write_data;
                MEM_WB(15 downto 0) <= var_data_mem(var_addr);
                
                -- passing the control flag mem-reg
                MEM_WB(16) <= EX_MEM(16);
            end if;
        end if;
                
                
       
        -- continuous read of the memory location given by var_addr 
        --data_out <= var_data_mem(var_addr);
 
        -- the following are probe signals (for simulation purpose) 
        sig_data_mem <= var_data_mem;

    end process;
  
end behavioral;
