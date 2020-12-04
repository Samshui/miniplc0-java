package c0.table;

import c0.analyser.SymbolEntry;

public class SymbolTable {
	private SymbolEntry[] table;
	private int usedCount;

	/**
	 * @param memAsk 申请的符号表存储空间大小
	 */
	public SymbolTable(int memAsk) {
		this.table = new SymbolEntry[memAsk];
		this.usedCount = 0;
	}

	public SymbolTable() {
		this.table = new SymbolEntry[10000];
		this.usedCount = 0;
	}

	/* setter & getter */
	public int getUsedCount() {
		return usedCount;
	}

	public void setUsedCount(int usedCount) {
		this.usedCount = usedCount;
	}

	/**
	 * 本表查询
	 *
	 * @param name
	 * @param type
	 * @return 是否存在（真值）
	 */
	private Boolean searchByNameAndType_BOOL(String name, int type) {
		if (usedCount > 0)
			for (int i = 0; i < usedCount; i++)
				if (table[i].getName().equals(name) && table[i].getType() == type)
					return true;
		// 空表或未查询到
		return false;
	}

	/**
	 * 本表查询
	 *
	 * @param name
	 * @param type
	 * @return 返回实体
	 */
	private SymbolEntry searchByNameAndType_ENTRY(String name, int type) {
		if (usedCount > 0)
			for (int i = 0; i < usedCount; i++)
				if (table[i].getName().equals(name) && table[i].getType() == type)
					return table[i];
		// 空表或未查询到
		return null;
	}
}
