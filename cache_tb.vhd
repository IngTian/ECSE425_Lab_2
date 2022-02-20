LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY cache_tb IS
END cache_tb;

ARCHITECTURE behavior OF cache_tb IS

    COMPONENT cache IS
        GENERIC (
            ram_size : INTEGER := 32768;
        );
        PORT (
            clock : IN STD_LOGIC;
            reset : IN STD_LOGIC;

            -- Avalon interface --
            s_addr : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
            s_read : IN STD_LOGIC;
            s_readdata : OUT STD_LOGIC_VECTOR (31 DOWNTO 0);
            s_write : IN STD_LOGIC;
            s_writedata : IN STD_LOGIC_VECTOR (31 DOWNTO 0);
            s_waitrequest : OUT STD_LOGIC;

            m_addr : OUT INTEGER RANGE 0 TO ram_size - 1;
            m_read : OUT STD_LOGIC;
            m_readdata : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
            m_write : OUT STD_LOGIC;
            m_writedata : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
            m_waitrequest : IN STD_LOGIC
        );
    END COMPONENT;

    COMPONENT memory IS
        GENERIC (
            ram_size : INTEGER := 32768;
            mem_delay : TIME := 10 ns;
            clock_period : TIME := 1 ns
        );
        PORT (
            clock : IN STD_LOGIC;
            writedata : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
            address : IN INTEGER RANGE 0 TO ram_size - 1;
            memwrite : IN STD_LOGIC;
            memread : IN STD_LOGIC;
            readdata : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
            waitrequest : OUT STD_LOGIC
        );
    END COMPONENT;

    -- test signals 
    SIGNAL reset : STD_LOGIC := '0';
    SIGNAL clk : STD_LOGIC := '0';
    CONSTANT clk_period : TIME := 1 ns;

    SIGNAL s_addr : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL s_read : STD_LOGIC;
    SIGNAL s_readdata : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL s_write : STD_LOGIC;
    SIGNAL s_writedata : STD_LOGIC_VECTOR (31 DOWNTO 0);
    SIGNAL s_waitrequest : STD_LOGIC;

    SIGNAL m_addr : INTEGER RANGE 0 TO 2147483647;
    SIGNAL m_read : STD_LOGIC;
    SIGNAL m_readdata : STD_LOGIC_VECTOR (7 DOWNTO 0);
    SIGNAL m_write : STD_LOGIC;
    SIGNAL m_writedata : STD_LOGIC_VECTOR (7 DOWNTO 0);
    SIGNAL m_waitrequest : STD_LOGIC;

BEGIN

    -- Connect the components which we instantiated above to their
    -- respective signals.
    dut : cache
    PORT MAP(
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
    PORT MAP(
        clock => clk,
        writedata => m_writedata,
        address => m_addr,
        memwrite => m_write,
        memread => m_read,
        readdata => m_readdata,
        waitrequest => m_waitrequest
    );
    clk_process : PROCESS
    BEGIN
        clk <= '0';
        WAIT FOR clk_period/2;
        clk <= '1';
        WAIT FOR clk_period/2;
    END PROCESS;

    test_process : PROCESS
    BEGIN

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
        WAIT FOR clk_period;

        -- 1. READ - Invalid, Non-dirty, Miss
        s_addr <= (OTHERS => '0');
        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"03020100" REPORT "TEST 1 FAILED! [Read,Invalid,Non-dirty,Miss]" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;

        -- 2. WRITE - Invalid, Non-dirty, Miss
        s_addr <= (OTHERS => '1');
        s_read <= '0';
        s_write <= '1';
        s_writedata <= x"AAAAAAAA";
        WAIT UNTIL falling_edge(s_waitrequest);

        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"AAAAAAAA" REPORT "TEST 2 FAILED! [Read,Invalid,Non-dirty,Miss]" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;

        -- 3. READ - Valid, Dirty, Hit
        s_addr <= (OTHERS => '1');
        s_read <= '0';
        s_write <= '1';
        s_writedata <= x"FFFFFFFF";
        WAIT UNTIL falling_edge(s_waitrequest);

        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"FFFFFFFF" REPORT "TEST 3 FAILED! [Read,Valid,Dirty,Hit]" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;

        -- 4. READ - Valid, Non-dirty, Hit
        s_addr <= (OTHERS => '0');
        s_read <= '1';
        s_write <= '0';
        s_writedata <= x"00000000";
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"FFFFFFF0" REPORT "TEST 4 FAILED! [Read,Valid,Non-dirty,Hit]" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;

        -- 5. READ - Valid, Non-dirty, Miss
        s_addr <= (9 => '1', OTHERS => '0');
        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"03020100" REPORT "TEST 5 FAILED! [Read,Valid,Non-dirty,Miss]" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;

        -- 6. READ - Valid, Dirty, Miss
        s_addr <= (14 => '0', OTHERS => '1');
        s_read <= '0';
        s_write <= '1';
        s_writedata <= x"0A0B0C0D";
        WAIT UNTIL falling_edge(s_waitrequest);

        s_addr <= (11 => '1', OTHERS => '0');
        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"0B0C0D00" REPORT "TEST 3 IS FAILED!" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;

        -- 9. WRTIE - Valid, Non-dirty, Hit
        s_addr <= (31 | 30 => '1', OTHERS => '0');
        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        s_read <= '0';
        s_write <= '1';
        s_writedata <= x"BBBBBBBB";
        WAIT UNTIL falling_edge(s_waitrequest);

        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"BBBBBBBB" REPORT "TEST 9 IS FAILED!" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;
        -- 11. WRTIE - Valid, Dirty, Hit
        s_addr <= (31 | 30 => '1', OTHERS => '0');
        s_read <= '0';
        s_write <= '1';
        s_writedata <= x"BBBBBBBB";
        WAIT UNTIL falling_edge(s_waitrequest);

        s_addr <= (31 | 30 => '1', OTHERS => '0');
        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;

        s_read <= '0';
        s_write <= '1';
        s_writedata <= x"CCCCCCCC";
        WAIT UNTIL falling_edge(s_waitrequest);

        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"CCCCCCCC" REPORT "TEST 11 IS FAILED!" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;
        -- 10. WRITE - Valid, Non-dirty, Miss
        s_addr <= (31 | 30 | 29 => '1', OTHERS => '0');
        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        s_addr <= (31 | 30 | 29 | 7 => '1', OTHERS => '0');
        s_read <= '0';
        s_write <= '1';
        s_writedata <= x"DDDDDDDD";
        WAIT UNTIL falling_edge(s_waitrequest);

        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"DDDDDDDD" REPORT "TEST 10 IS FAILED!" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;
        -- 12. WRITE - Valid, Dirty, Miss
        s_addr <= (31 | 30 | 29 | 28 => '1', OTHERS => '0');
        s_read <= '0';
        s_write <= '1';
        s_writedata <= x"00000000";
        WAIT UNTIL falling_edge(s_waitrequest);

        s_addr <= (31 | 30 | 29 | 28 | 7 => '1', OTHERS => '0');
        s_read <= '0';
        s_write <= '1';
        s_writedata <= x"11111111";
        WAIT UNTIL falling_edge(s_waitrequest);

        s_read <= '1';
        s_write <= '0';
        WAIT UNTIL falling_edge(s_waitrequest);

        ASSERT s_readdata = x"11111111" REPORT "TEST 10 IS FAILED!" SEVERITY error;

        s_read <= '0';
        s_write <= '0';
        WAIT FOR clk_period;

        WAIT;

    END PROCESS;

END;