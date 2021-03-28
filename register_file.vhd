
  
---------------------------------------------------------------------------
-- register_file.vhd - Implementation of A Dual-Port, 16 x 16-bit
--                     Collection of Registers.
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

entity register_file is
--    port ( reset           : in  std_logic;
--           clk             : in  std_logic;
--           read_register_a : in  std_logic_vector(3 downto 0);
--           read_register_b : in  std_logic_vector(3 downto 0);
--           write_enable    : in  std_logic;
--           write_register  : in  std_logic_vector(3 downto 0);
--           write_data      : in  std_logic_vector(15 downto 0);
--           read_data_a     : out std_logic_vector(15 downto 0);
--           read_data_b     : out std_logic_vector(15 downto 0) );

 port ( reset           : in  std_logic;
           clk             : in  std_logic;
           IF_ID           : in std_logic_vector(19 downto 0);
           write_enable    : in  std_logic;
           write_data      : in  std_logic_vector(15 downto 0);
           ID_EX            : out std_logic_vector(60 downto 0);
           regb             : out std_logic_vector(15 downto 0) );
           
--            reset           => reset, 
--               clk             => clk,
--               IF_ID           => IF_ID
--               write_enable    => sig_reg_write,
--               write_data      => sig_write_data,
--               ID_EX           => ID_EX );
end register_file;

architecture behavioral of register_file is

type reg_file is array(0 to 15) of std_logic_vector(15 downto 0);
signal sig_regfile : reg_file;

begin

    mem_process : process ( reset,
                            clk,
                            --read_register_a,
                            --read_register_b,
                            IF_ID,
                            write_enable,
                            --write_register,
                            write_data ) is

    variable var_regfile     : reg_file;
    variable var_read_addr_a : integer;
    variable var_read_addr_b : integer;
    variable var_write_addr  : integer;
    
    begin
    
--        var_read_addr_a := conv_integer(read_register_a);
--        var_read_addr_b := conv_integer(read_register_b);
--        var_write_addr  := conv_integer(write_register);
        
        
        
      
        if (reset = '1') then
            -- initial values of the registers - reset to zeroes
            var_regfile := (others => X"0000");
            
        -- read IF_ID on rising edge 
        else 
            if (rising_edge(clk)) then 
              var_read_addr_a := conv_integer(IF_ID(11 downto 8));
              var_read_addr_b := conv_integer(IF_ID(7 downto 4));
              var_write_addr  := conv_integer(IF_ID(3 downto 0));
            end if;
          
        -- write to ID_EX on falling edge  
            if (falling_edge(clk)) then
            -- register write on the falling clock edge
                if (write_enable = '1') then
                    var_regfile(var_write_addr) := write_data;
                end if;
            ID_EX(47 downto 32) <= var_regfile(var_read_addr_a); 
            regb <= var_regfile(var_read_addr_b);
            ID_EX(15 downto 0) <= var_regfile(var_write_addr);
            ID_EX(51 downto 48) <= IF_ID(15 downto 112);
            ID_EX(55 downto 52) <= IF_ID(19 downto 16);
            
           end if;
         end if;
             
        

        -- enforces value zero for register $0
        var_regfile(0) := X"0000";

        -- continuous read of the registers at location read_register_a
        -- and read_register_b
        --read_data_a <= 
--        read_data_b <= var_regfile(var_read_addr_b);

        -- the following are probe signals (for simulation purpose)
        

    end process; 
end behavioral;

