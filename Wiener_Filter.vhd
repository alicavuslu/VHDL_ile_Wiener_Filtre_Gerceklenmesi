library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_SIGNED.ALL;
use IEEE.MATH_REAL.ALL;
use std.textio.ALL;

library work;
use work.Wiener_Filter_Package.all;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx leaf cells in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity Wiener_Filter is
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
end Wiener_Filter;

architecture Behavioral of Wiener_Filter is

  type t_Signal_Buf_vec is array(0 to c_TIME_samp - 1) of std_logic_vector(DATA_LENGTH - 1 downto 0);

  type t_X_Shift_Ctrl is (IDLE, SHIFT_DATA);
	signal r_X_Shift_Ctrl : t_X_Shift_Ctrl := IDLE;

  signal r_calc_fltr_out_strt : std_logic := '0';
	signal r_data : std_logic_vector(DATA_LENGTH - 1 downto 0) := (others => '0');
	signal r_filter_out_vld : std_logic := '0';
	signal n_i : integer := 0;

  type t_data_X_buffer is array (0 to c_FILT_LEN - 1 ) of std_logic_vector(DATA_LENGTH - 1 downto 0);
  signal r_data_X_buffer : t_data_X_buffer := (others => (others => '0')); 

  function f_Shift_X_Buf(r_data_X_buffer : t_data_X_buffer; in_data : std_logic_vector(DATA_LENGTH - 1 downto 0)) return t_data_X_buffer is
     variable v_data_X_buffer : t_data_X_buffer;
  begin
    v_data_X_buffer := r_data_X_buffer;
    for n_i in c_FILT_LEN - 2 downto 0 loop
      v_data_X_buffer(n_i + 1) := v_data_X_buffer(n_i); 
    end loop;
    v_data_X_buffer(0) := in_data; 
    return v_data_X_buffer;        
  end f_Shift_X_Buf;  

  signal r_filter_sum_out : std_logic_vector(COEF_LENGTH + COEF_ADD + DATA_LENGTH + 4 - 1 downto 0) := (others => '0');
	signal r_filter_out : std_logic_vector(DATA_LENGTH - 1 downto 0) := (others => '0');
	
  type t_Calc_Filter_out is (IDLE, ADD_X_BUF_PARAM , SUB_Y_BUF_PARAM, ROUND_PROCESS, DONE);
	signal r_Calc_Filter_out : t_Calc_Filter_out := IDLE;
  


begin
	out_data <= r_filter_out;
  out_data_vld <= r_filter_out_vld; 
   
  process(in_clk, in_rst)
  begin
    if in_rst = '1' then
      r_data_X_buffer <= (others => (others => '0')); 
			r_calc_fltr_out_strt <= '0';
			r_data <= (others => '0');
			
    elsif rising_edge(in_clk) then
			r_calc_fltr_out_strt <= '0';
      case r_X_Shift_Ctrl is
        when IDLE => 
         if in_data_vld = '1' then
           r_data <= in_data;
           r_X_Shift_Ctrl <= SHIFT_DATA;
         end if;
                    
        when SHIFT_DATA => 
          r_data_X_buffer <= f_Shift_X_Buf(r_data_X_buffer, r_data);
          r_X_Shift_Ctrl <= IDLE; 
					r_calc_fltr_out_strt <= '1';
        when others => NULL;    
      end case;
    end if;
  end process;
	
	process(in_clk, in_rst)
	begin
		if in_rst = '1' then
			r_Calc_Filter_out <= IDLE;
			r_filter_sum_out <= (others => '0');
			r_filter_out_vld <= '0';
			r_filter_out <= (others => '0');
			n_i <= 0; 

		elsif rising_edge(in_clk) then
			r_filter_out_vld <= '0';
			case r_Calc_Filter_out is
				when IDLE =>
					if r_calc_fltr_out_strt = '1' then
						r_Calc_Filter_out <= ADD_X_BUF_PARAM; 
					end if;
				
				when ADD_X_BUF_PARAM =>
					r_filter_sum_out <= r_filter_sum_out + sxt((r_data_X_buffer(n_i)) * (r_Coef_vec(n_i)), r_filter_sum_out'length);
					n_i <= n_i + 1;
					if n_i = c_FILT_LEN-1 then
						n_i <= 0;
						r_Calc_Filter_out <= DONE; 
					end if;
					
				when DONE =>		
					r_filter_out_vld <= '1';
					r_filter_sum_out <= (others => '0');
					r_filter_out <= r_filter_sum_out(COEF_LENGTH + DATA_LENGTH - 1 downto COEF_LENGTH );
					r_Calc_Filter_out <= IDLE; 
				when others => NULL;
			end case;
		end if;
	end process;

end Behavioral;
