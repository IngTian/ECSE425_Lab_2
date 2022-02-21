LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

ENTITY cache IS
	GENERIC (
		ram_size : INTEGER := 32768;
		cache_size : INTEGER := 4096;
		word_num : INTEGER := 128;
		block_num : INTEGER := 32;
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
		s_waitrequest : OUT STD_LOGIC := '0';

		m_addr : OUT INTEGER RANGE 0 TO ram_size - 1;
		m_read : OUT STD_LOGIC;
		m_readdata : IN STD_LOGIC_VECTOR (7 DOWNTO 0);
		m_write : OUT STD_LOGIC;
		m_writedata : OUT STD_LOGIC_VECTOR (7 DOWNTO 0);
		m_waitrequest : IN STD_LOGIC
	);
END cache;

ARCHITECTURE arch OF cache IS
	TYPE CACHE_STORAGE IS ARRAY(word_num - 1 DOWNTO 0) OF STD_LOGIC_VECTOR(31 DOWNTO 0);
	-- We define the checkup vector to have the following structure.
	-- 0th to 5th bits are for tags.
	-- 6th bit is the dirty bit.
	-- 7th bit is the invalid bit.
	TYPE CACHE_CHECKUP IS ARRAY(block_num - 1 DOWNTO 0) OF STD_LOGIC_VECTOR(7 DOWNTO 0);
	TYPE STATE_TYPE IS (
		IDLE,
		VALID_AND_MATCH,
		MISMATCH_AND_NON_DIRTY,
		MISMATCH_AND_DIRTY,
		MEMORY_READ_WAIT,
		MEMORY_WRITE_WAIT,
		INVALID
	);
	SIGNAL cache_memory : CACHE_STORAGE;
	SIGNAL cache_checkup_table : CACHE_CHECKUP;
	SIGNAL current_state : state_type := IDLE;
	SIGNAL request_block_idx : INTEGER RANGE 0 TO 31 := 0;
	SIGNAL request_tag : STD_LOGIC_VECTOR(5 DOWNTO 0);
	SIGNAL request_word_idx : INTEGER RANGE 0 TO 3 := 0;
	SIGNAL request_addr : STD_LOGIC_VECTOR (31 DOWNTO 0);
BEGIN
	cache_process : PROCESS (clock, reset)
		VARIABLE mem_read_addr : INTEGER RANGE 0 TO ram_size;
		VARIABLE mem_write_addr : INTEGER RANGE 0 TO ram_size;
		VARIABLE current_rw_byte_count : INTEGER RANGE 0 TO 4;
		VARIABLE current_rw_word_count : INTEGER RANGE 0 TO 4;
	BEGIN
		-- Initialize the cache by marking
		-- all blocks invalid.
		IF (now < 1 ps) THEN
			s_waitrequest <= '0';
			m_read <= '0';
			m_write <= '0';
			FOR i IN 0 TO block_num - 1 LOOP
				cache_checkup_table(i) <= "11000000"; -- Invalid, non-dirty, with 0 tag.
			END LOOP;
		END IF;

		-- In case of the reset,
		-- simply re-initialize.
		IF (reset'event AND reset = '1') THEN
			FOR i IN 0 TO block_num - 1 LOOP
				cache_checkup_table(i) <= "11000000"; -- Invalid, non-dirty, with 0 tag.
			END LOOP;

			-- If this is not a reset signal,
			-- we go into the state machine.
		ELSIF (clock'event AND clock = '1') THEN
			CASE current_state IS
				WHEN IDLE =>
					-- In the idle state, we should
					-- inquire on the s_read and s_write
					-- status. If both signals are idle,
					-- there is no operation, and we do
					-- nothing. If either of them
					-- is high, we shall proceed to other
					-- states.
					IF ((s_write = '1') OR (s_read = '1')) THEN
						-- If either works, we look into the specific address and
						-- decide on dirty/non-dirty, match/mismatch, and valid/invalid
						-- of the word being required.

						s_waitrequest <= '1'; -- Tell the core to wait.

						-- Record address components for convenience.
						request_tag <= s_addr(14 DOWNTO 9);
						request_block_idx <= to_integer(unsigned(s_addr(8 DOWNTO 4)));
						request_word_idx <= to_integer(unsigned(s_addr(3 DOWNTO 2)));
						request_addr <= s_addr;

						IF (cache_checkup_table(request_block_idx)(7) = '1') THEN -- The requested block is invalid.
							current_state <= INVALID;
						ELSIF (cache_checkup_table(request_block_idx)(5 DOWNTO 0) = s_addr(14 DOWNTO 9)) THEN -- The requested block is valid and match.
							current_state <= VALID_AND_MATCH;
						ELSIF (cache_checkup_table(request_block_idx)(6) = '0') THEN -- The requested block is valid, mis-matched, and dirty.
							current_state <= MISMATCH_AND_DIRTY;
						ELSIF (cache_checkup_table(request_block_idx)(6) = '1') THEN -- The requested block valid, mis-matched, and non-dirty.
							current_state <= MISMATCH_AND_NON_DIRTY;
						END IF;
					END IF;
				WHEN VALID_AND_MATCH =>
					-- If the block requested is valid and matched,
					-- we shall copy the data from the cache into
					-- the reg in the case of a read, and write the requested
					-- word to the location and mark the block dirty.

					IF (s_read = '1') THEN -- Read case.
						s_readdata <= cache_memory(request_block_idx * 4 + request_word_idx);
					ELSIF (s_write = '1') THEN -- Write case.
						cache_memory(request_block_idx * 4 + request_word_idx) <= s_writedata;
						cache_checkup_table(request_block_idx)(6) <= '0';
					END IF;

					s_waitrequest <= '0'; -- Tell the core to proceed.
					current_state <= IDLE;

				WHEN MISMATCH_AND_NON_DIRTY =>
					-- If the requested block is non-dirty and mismatched,
					-- we shall write data back to memory.
					current_state <= MEMORY_READ_WAIT;
					mem_read_addr := to_integer(unsigned(s_addr(14 DOWNTO 4) & "0000"));
					current_rw_byte_count := 0;
					current_rw_word_count := 0;
				WHEN MISMATCH_AND_DIRTY =>
					-- If te requested block is dirty and mismatched,
					-- we shall write data back to memory.
					current_state <= MEMORY_WRITE_WAIT;
					mem_read_addr := to_integer(unsigned(s_addr(14 DOWNTO 4) & "0000"));
					mem_write_addr := to_integer(unsigned(cache_memory(request_block_idx * 4 + request_word_idx)(5 DOWNTO 0) & s_addr(8 DOWNTO 4) & "0000"));
					current_rw_byte_count := 0;
					current_rw_word_count := 0;
				WHEN MEMORY_READ_WAIT =>
					IF (m_read = '0') THEN
						IF (current_rw_word_count = 4) THEN -- completed.
							cache_checkup_table(request_block_idx)(7) <= '0'; -- Making this block valid.
							cache_checkup_table(request_block_idx)(6) <= '1'; -- Making this block non-dirty.
							cache_checkup_table(request_block_idx)(5 DOWNTO 0) <= request_tag; -- Update its tag.
							current_state <= VALID_AND_MATCH;
							current_rw_byte_count := 0;
							mem_read_addr := 0;
							current_rw_word_count := 0;
						ELSE -- Write data into the buffer, and initiate request.
							m_read <= '1';
							m_addr <= mem_read_addr;
						END IF;
					ELSIF (m_waitrequest = '0') THEN -- Memory completed, update operation.
						mem_read_addr := mem_read_addr + 1;
						m_read <= '0';
						cache_memory(request_block_idx * 4 + current_rw_word_count)(current_rw_byte_count * 8 + 7 DOWNTO current_rw_byte_count * 8) <= m_readdata;
						current_rw_byte_count := current_rw_byte_count + 1;
						IF (current_rw_byte_count = 4) THEN
							current_rw_byte_count := 0;
							current_rw_word_count := current_rw_word_count + 1;
						END IF;
					END IF;
				WHEN MEMORY_WRITE_WAIT =>

					IF (m_write = '0') THEN
						IF (current_rw_word_count = 4) THEN -- completed.
							current_state <= MEMORY_READ_WAIT;
							current_rw_byte_count := 0;
							current_rw_word_count := 0;
							mem_write_addr := 0;
						ELSE -- Write data into the buffer, and initiate request.
							m_write <= '1';
							m_addr <= mem_write_addr;
							m_writedata <= cache_memory(request_block_idx * 4 + current_rw_word_count)(current_rw_byte_count * 8 + 7 DOWNTO current_rw_byte_count * 8);
						END IF;
					ELSIF (m_waitrequest = '0') THEN -- Memory completed, update operation.
						mem_write_addr := mem_write_addr + 1;
						current_rw_byte_count := current_rw_byte_count + 1;
						m_write <= '0';
						IF (current_rw_byte_count = 4) THEN
							current_rw_byte_count := 0;
							current_rw_word_count := current_rw_word_count + 1;
						END IF;
					END IF;

				WHEN INVALID =>
					-- If the requested block is invalid,
					-- we are going to fetch contents from
					-- memory no matter what. 
					current_state <= MEMORY_READ_WAIT;
					mem_read_addr := to_integer(unsigned(s_addr(14 DOWNTO 4) & "0000"));
					current_rw_byte_count := 0;
					current_rw_word_count := 0;
			END CASE;
		END IF;

	END PROCESS;

END arch;