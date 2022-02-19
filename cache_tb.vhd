library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity cache_tb is
end cache_tb;

architecture behavior of cache_tb is

component cache is
generic(
    ram_size : INTEGER := 32768;
);
port(
    clock : in std_logic;
    reset : in std_logic;

    -- Avalon interface --
    s_addr : in std_logic_vector (31 downto 0);
    s_read : in std_logic;
    s_readdata : out std_logic_vector (31 downto 0);
    s_write : in std_logic;
    s_writedata : in std_logic_vector (31 downto 0);
    s_waitrequest : out std_logic; 

    m_addr : out integer range 0 to ram_size-1;
    m_read : out std_logic;
    m_readdata : in std_logic_vector (7 downto 0);
    m_write : out std_logic;
    m_writedata : out std_logic_vector (7 downto 0);
    m_waitrequest : in std_logic
);
end component;

component memory is 
GENERIC(
    ram_size : INTEGER := 32768;
    mem_delay : time := 10 ns;
    clock_period : time := 1 ns
);
PORT (
    clock: IN STD_LOGIC;
    writedata: IN STD_LOGIC_VECTOR (7 DOWNTO 0);
    address: IN INTEGER RANGE 0 TO ram_size-1;
    memwrite: IN STD_LOGIC;
    memread: IN STD_LOGIC;
    readdata: OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
    waitrequest: OUT STD_LOGIC
);
end component;
	
-- test signals 
signal reset : std_logic := '0';
signal clk : std_logic := '0';
constant clk_period : time := 1 ns;

signal s_addr : std_logic_vector (31 downto 0);
signal s_read : std_logic;
signal s_readdata : std_logic_vector (31 downto 0);
signal s_write : std_logic;
signal s_writedata : std_logic_vector (31 downto 0);
signal s_waitrequest : std_logic;

signal m_addr : integer range 0 to 2147483647;
signal m_read : std_logic;
signal m_readdata : std_logic_vector (7 downto 0);
signal m_write : std_logic;
signal m_writedata : std_logic_vector (7 downto 0);
signal m_waitrequest : std_logic; 

begin

-- Connect the components which we instantiated above to their
-- respective signals.
dut: cache 
port map(
    clock => clk,
    reset => reset,

    s_addr => s_addr,
    s_read => s_read,
    s_readdata => s_readdata,
    s_write => s_write,
    s_writedata => s_writedata,
    s_waitrequest => s_waitrequest,

    m_addr => m_addr,
    m_read => m_read,
    m_readdata => m_readdata,
    m_write => m_write,
    m_writedata => m_writedata,
    m_waitrequest => m_waitrequest
);

MEM : memory
port map (
    clock => clk,
    writedata => m_writedata,
    address => m_addr,
    memwrite => m_write,
    memread => m_read,
    readdata => m_readdata,
    waitrequest => m_waitrequest
);
				

clk_process : process
begin
  clk <= '0';
  wait for clk_period/2;
  clk <= '1';
  wait for clk_period/2;
end process;

test_process : process
begin

	-- Test Cases:
   	-- READ:
    --	1.  Valid	, 	Non-dirty	,	Hit  
	-- 	2.  Valid	, 	Non-dirty	,	Miss 
	-- 	3.  Valid	, 	Dirty	 	,	Hit  
	-- 	4.  Valid	,	Dirty		,	Miss 
	-- 	5.  Invalid	,	Non-dirty	, 	Hit  (Impossible)
	-- 	6.  Invalid	,	Non-dirty	, 	Miss 
	-- 	7.  Invalid	,	Dirty		, 	Hit  (Impossible)
	-- 	8.  Invalid	, 	Dirty		, 	Miss (Impossible)
    
   	-- WRITE:
    --	9.  Valid	, 	Non-dirty	,	Hit  
	-- 	10. Valid	, 	Non-dirty	,	Miss 
	-- 	11. Valid	, 	Dirty	 	,	Hit  
	-- 	12. Valid	,	Dirty		,	Miss 
	-- 	13. Invalid	,	Non-dirty	, 	Hit  (Impossible)
	-- 	14. Invalid	,	Non-dirty	, 	Miss 
	-- 	15. Invalid	,	Dirty		, 	Hit  (Impossible)
	-- 	16. Invalid	, 	Dirty		, 	Miss (Impossible)

	--------------------------------------------
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;
    
    
    -- 6. READ - Invalid, Non-dirty, Miss
    s_addr <= (others => '0');
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;
    
	
    -- 14. WRITE - Invalid, Non-dirty, Miss
    s_addr <= (others => '1');
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"AAAAAAAA";
    wait until falling_edge(s_waitrequest);
    
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    assert s_readdata = x"AAAAAAAA" report "TEST 14 IS FAILED!" severity error;
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;
    
    
    -- 1. READ - Valid, Non-dirty, Hit
    s_addr <= (31 => '1', others => '0');
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"FFFFFFFF";
    wait until falling_edge(s_waitrequest);
    
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    assert s_readdata = x"FFFFFFFF" report "TEST 1 IS FAILED!" severity error;
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;
    
    
    -- 3. READ - Valid, Dirty, Hit
    s_addr <= (31 => '1', others => '0');
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"FFFFFFF0";
    wait until falling_edge(s_waitrequest);
    
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    assert s_readdata = x"FFFFFFF0" report "TEST 3 IS FAILED!" severity error;
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;
    
    
    -- 2. READ - Valid, Non-dirty, Miss
    s_addr <= (others => '0');
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    s_addr <= (7 => '1', others => '0');
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;

    
	-- 4. READ - Valid, Dirty, Miss
    s_addr <= (31|30|29|28|27|26|25 => '1', others => '0');
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"0A0B0C0D";
    wait until falling_edge(s_waitrequest);
    
    s_addr <= (11 => '1', others => '0');
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    assert s_readdata = x"0B0C0D00" report "TEST 3 IS FAILED!" severity error;
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;
    
    
	-- 9. WRTIE - Valid, Non-dirty, Hit
    s_addr <= (31|30 => '1', others => '0');
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"BBBBBBBB";
    wait until falling_edge(s_waitrequest);
    
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    assert s_readdata = x"BBBBBBBB" report "TEST 9 IS FAILED!" severity error;
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;
    
    
    -- 11. WRTIE - Valid, Dirty, Hit
    s_addr <= (31|30 => '1', others => '0');    
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"BBBBBBBB";
    wait until falling_edge(s_waitrequest);
    
    s_addr <= (31|30 => '1', others => '0');
     s_read <= '0';
    s_write <= '0';
    wait for clk_period;
        
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"CCCCCCCC";
    wait until falling_edge(s_waitrequest);
    
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    assert s_readdata = x"CCCCCCCC" report "TEST 11 IS FAILED!" severity error;
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;
    
    
    -- 10. WRITE - Valid, Non-dirty, Miss
    s_addr <= (31|30|29 => '1', others => '0');
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    s_addr <= (31|30|29|7 => '1', others => '0');
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"DDDDDDDD";
    wait until falling_edge(s_waitrequest);
    
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    assert s_readdata = x"DDDDDDDD" report "TEST 10 IS FAILED!" severity error;
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;
    
    
    -- 12. WRITE - Valid, Dirty, Miss
    s_addr <= (31|30|29|28 => '1', others => '0');
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"00000000";
    wait until falling_edge(s_waitrequest);
    
    s_addr <= (31|30|29|28|7 => '1', others => '0');
    s_read <= '0';
    s_write <= '1';
    s_writedata <= x"11111111";
    wait until falling_edge(s_waitrequest);
    
    s_read <= '1';
    s_write <= '0';
    wait until falling_edge(s_waitrequest);
    
    assert s_readdata = x"11111111" report "TEST 10 IS FAILED!" severity error;
    
    s_read <= '0';
    s_write <= '0';
    wait for clk_period;

	wait;
	
end process;
	
end;