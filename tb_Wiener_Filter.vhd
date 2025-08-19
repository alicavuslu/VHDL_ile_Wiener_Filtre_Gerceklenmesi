
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity tb_Wiener_Filter is
end tb_Wiener_Filter;

architecture Behavioral of tb_Wiener_Filter is

    component Wiener_Filter
    Generic(
        DATA_LENGTH : integer := 16
    );
    Port ( 
        in_clk : in std_logic;
        in_rst : in std_logic;
        in_data : in std_logic_vector(DATA_LENGTH - 1 downto 0);
        in_data_vld : in std_logic;
        out_data : out std_logic_vector(DATA_LENGTH - 1 downto 0);
        out_data_vld : out std_logic

    );  
    end component;
    
    constant DATA_PATH : string := "D:\Sum_Signal.txt";
    
    constant CLK_PERIOD : time := 100 ns;    
    constant SAMPLING_CNTR : integer := 1000;
    constant DATA_LENGTH : integer := 24;

    signal in_clk : std_logic  := '0';
    signal clk_cnt : integer := 0;

    signal in_data : std_logic_vector(DATA_LENGTH - 1 downto 0) := (others => '0');
    signal in_data_vld : std_logic := '0';

    signal out_data : std_logic_vector(DATA_LENGTH - 1 downto 0) := (others => '0');
    signal out_data_vld : std_logic := '0';

begin

  process
  begin
    in_clk <= '1';
    wait for CLK_PERIOD / 2;
    in_clk <= '0';
    wait for CLK_PERIOD / 2;      
  end process;

  process (in_clk)
    file file_s : text open read_mode is DATA_PATH ;
    variable line_s : line;
    variable data_s : real;
  begin 
    if rising_edge(in_clk) then
        in_data_vld <= '0';
        if clk_cnt = SAMPLING_CNTR - 1 then
            clk_cnt <= 0;
            if not(endfile(file_s)) then
                readline(file_s, line_s);
                read(line_s, data_s);
                in_data <= conv_std_logic_vector(integer(data_s * real(2**(DATA_LENGTH - 2))), 24) ;               
                in_data_vld <= '1';
            end if;             
        else
            clk_cnt <= clk_cnt + 1;
        end if;      
    end if;
  end process;

    Wiener_Filter_map : Wiener_Filter
    Generic map(
        DATA_LENGTH => DATA_LENGTH
    )
    Port map( 
        in_clk => in_clk,
        in_rst => '0',
        in_data => in_data,
        in_data_vld => in_data_vld,
        out_data => out_data,
        out_data_vld => out_data_vld
    );  
    

end Behavioral;
