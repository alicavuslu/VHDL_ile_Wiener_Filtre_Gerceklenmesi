----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date: 14.06.2025 22:22:15
-- Design Name: 
-- Module Name: Wiener_Filter_Package - Behavioral
-- Project Name: 
-- Target Devices: 
-- Tool Versions: 
-- Description: 
-- 
-- Dependencies: 
-- 
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
-- 
----------------------------------------------------------------------------------

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

package Wiener_Filter_Package is

  constant c_FILT_LEN :  integer := 32;
  constant c_FS : integer := 10_000; -- Hz - Örnekleme Frekansı
  constant c_TIME : integer := 100; -- ms - Referasn sinyal süresi
  constant c_FREQ_0 : integer := 2000; -- Hz referans sinyal frekansı
  constant c_TIME_samp : integer := c_FS * c_TIME / 1000; -- Referans sinyal oluşturmak gerekli örnek sayısı
  constant COEF_LENGTH : integer := 12;
  constant COEF_ADD : integer := 4;

  constant PATH_REF : string := "D:\Ref_Signal.txt";
  constant PATH_SUM : string := "D:\Sum_Signal.txt";
  constant PATH_COEF : string := "D:\Wiener_Coef.txt";


  type t_Signal_Buf is array(0 to c_TIME_samp - 1) of real;

  type t_Rxx_Buf is array(0 to c_FILT_LEN - 1, 0 to c_FILT_LEN - 1) of real;
  type t_Rxd_Buf is array(0 to c_FILT_LEN - 1) of real;
  type t_Ext_Rxx_Buf is array(0 to c_FILT_LEN - 1, 0 to c_FILT_LEN) of real;

  function f_Rand_Noise(f_TIME_samp : integer) return t_Signal_Buf;
  function f_Ref_Sin(f_FS, f_TIME_samp, f_FREQ : integer) return t_Signal_Buf; 
  function f_Sum_Signal(Ref_Sig, Rand_Noise : t_Signal_Buf; f_TIME_samp : integer) return t_Signal_Buf;

  signal r_Ref_Signal : t_Signal_Buf := f_Ref_Sin(c_FS, c_TIME_samp, c_FREQ_0);
  signal r_Noise_Signal : t_Signal_Buf := f_Rand_Noise(c_TIME_samp);
  signal r_Sum_Signal : t_Signal_Buf := f_Sum_Signal(r_Ref_Signal, r_Noise_Signal, c_TIME_samp);

  function f_Extend_Rxx(Rxx : t_Rxx_Buf; Rxd : t_Rxd_Buf; Filt_Len : integer) return t_Ext_Rxx_Buf;

  function f_Coef_Calc(Sum_Sig, Ref_Sig : t_Signal_Buf; Filt_Len, Data_Len : integer) return t_Rxd_Buf;
  signal r_Coef : t_Rxd_Buf := f_Coef_Calc(r_Sum_Signal, r_Ref_Signal, c_FILT_LEN, c_TIME_samp);

  type t_Rxd_Buf_vec is array(0 to c_FILT_LEN - 1) of std_logic_vector(COEF_LENGTH + COEF_ADD - 1 downto 0);
  function f_Coef_Conv( Rxd : t_Rxd_Buf; Filt_Len : integer) return t_Rxd_Buf_vec;
  
  signal r_Coef_vec : t_Rxd_Buf_vec := f_Coef_Conv(r_Coef, c_FILT_LEN);

end Wiener_Filter_Package;

package body Wiener_Filter_Package is
  function f_Rand_Noise(f_TIME_samp : integer) return t_Signal_Buf is
    variable v_Noise_Signal : t_Signal_Buf := (others => 0.0);
    variable seed1 : integer := 1;
    variable seed2 : integer := 1;
    variable r : real;
    variable min_val : real := -1.0;
    variable max_val : real :=  1.0;
  begin
    for ni in 0 to f_TIME_samp - 1 loop 
      uniform(seed1, seed2, r);
      v_Noise_Signal(ni) := r * (max_val - min_val) + min_val;
    end loop;
    return v_Noise_Signal;
  end function;

  function f_Ref_Sin(f_FS, f_TIME_samp, f_FREQ : integer) return t_Signal_Buf is 
    variable v_Ref_Signal : t_Signal_Buf := (others => 0.0);
  begin
    for ni in 0 to f_TIME_samp - 1 loop
      v_Ref_Signal(ni) := sin(2.0 * MATH_PI * real(ni) * real(f_FREQ) / real(f_FS));
    end loop;
    return v_Ref_Signal;
  end function;

  function f_Sum_Signal(Ref_Sig, Rand_Noise : t_Signal_Buf; f_TIME_samp : integer) return t_Signal_Buf is 
    variable v_Sum_Signal : t_Signal_Buf := (others => 0.0);
  begin
      for ni in 0 to f_TIME_samp - 1 loop
        v_Sum_Signal(ni) := Ref_Sig(ni) + Rand_Noise(ni);
      end loop;
    return v_Sum_Signal;
  end function;

  function f_Extend_Rxx(Rxx : t_Rxx_Buf; Rxd : t_Rxd_Buf; Filt_Len : integer) return t_Ext_Rxx_Buf is
    variable v_Ext_Rxx_Buf : t_Ext_Rxx_Buf;
  begin

    for ni in 0 to Filt_Len - 1 loop
      for nj in 0 to Filt_Len loop    
        if nj = Filt_Len then
          v_Ext_Rxx_Buf(ni, nj) := Rxd(ni);
        else
          v_Ext_Rxx_Buf(ni, nj) := Rxx(ni, nj);
        end if;
      end loop;
    end loop;
    return v_Ext_Rxx_Buf;
  end function;
  

  function f_Coef_Calc(Sum_Sig, Ref_Sig : t_Signal_Buf; Filt_Len, Data_Len : integer) return t_Rxd_Buf is
    variable v_Rxx : t_Rxx_Buf := (others => (others => 0.0));
    variable v_Rxd : t_Rxd_Buf := (others => 0.0);
    variable v_Ext_Rxx : t_Ext_Rxx_Buf;
    variable v_factor : real;
    variable v_Coef : t_Rxd_Buf := (others => 0.0);
    variable v_sum : real := 0.0;

    
    file file_s : text open write_mode is PATH_SUM;
    file file_c : text open write_mode is PATH_COEF;
    file file_r : text open write_mode is PATH_REF;
    
    variable row_r : line;
    variable row_s : line;
    variable row_c : line;
    variable data : real;

  begin
    for ni in 0 to Filt_Len - 1 loop
      for nj in 0 to Filt_Len - 1 loop
        for nn in Filt_Len - 1 to Data_Len - 1 loop
          v_Rxx(ni, nj) := v_Rxx(ni, nj) + Sum_Sig(nn - ni) * Sum_Sig(nn - nj);         
        end loop;
      end loop;
      for nn in Filt_Len - 1 to Data_Len - 1 loop
         v_Rxd(ni) := v_Rxd(ni) + Sum_Sig(nn - ni) * Ref_Sig(nn);
      end loop;      
    end loop;
    
    for ni in 0 to Filt_Len - 1 loop
      for nj in 0 to Filt_Len - 1 loop    
        v_Rxx(ni, nj) := v_Rxx(ni, nj) / real(Data_Len - Filt_Len + 1);
      end loop;
      v_Rxd(ni) := v_Rxd(ni) / real(Data_Len - Filt_Len + 1);
    end loop;
    
    v_Ext_Rxx := f_Extend_Rxx(v_Rxx, v_Rxd, Filt_Len);
    for ni in 0 to Filt_Len - 1 loop
      for nj in ni + 1 to Filt_Len - 1 loop  
        v_factor := v_Ext_Rxx(nj, ni) / v_Ext_Rxx(ni, ni);
        for nn in 0 to Filt_Len loop
          v_Ext_Rxx(nj, nn) := v_Ext_Rxx(nj, nn) - v_factor * v_Ext_Rxx(ni, nn);
        end loop;
      end loop;
    end loop;

    for ni in Filt_Len-1 downto 0 loop
      v_sum := 0.0;
      for nk in ni + 1 to Filt_Len - 1 loop
         v_sum := v_sum + v_Ext_Rxx(ni, nk) * v_Coef(nk);
      end loop;
      v_Coef(ni) := (v_Ext_Rxx(ni, Filt_Len) - v_sum) / v_Ext_Rxx(ni, ni);
    end loop;

    for ni in 0 to Filt_Len - 1 loop
        data := v_Coef(ni);
        write(row_c, data);
        writeline(file_c, row_c);    
      
    end loop;

    for ni in 0 to Data_Len - 1 loop
        data := Sum_Sig(ni);
        write(row_s, data);
        writeline(file_s, row_s);         
    end loop;
    for ni in 0 to Data_Len - 1 loop
        data := Ref_Sig(ni);
        write(row_r, data);
        writeline(file_r, row_r);         
    end loop;

    return v_Coef;

  end function;
  
  function f_Coef_Conv( Rxd : t_Rxd_Buf; Filt_Len : integer) return t_Rxd_Buf_vec is
      variable v_Rxd_vec : t_Rxd_Buf_vec;
  begin

    for ni in 0 to Filt_Len - 1 loop
      v_Rxd_vec(ni) := conv_std_logic_vector(integer(Rxd(ni) * real(2**COEF_LENGTH)), COEF_LENGTH + COEF_ADD);
    end loop;
    return v_Rxd_vec;
  end function;


end Wiener_Filter_Package;
