package c0.table;

import c0.analyser.FuncEntry;
import c0.analyser.SymbolEntry;

public class FuncTable {
	private FuncEntry[] table = new FuncEntry[100];
	private int funcCount;

	public FuncTable() {
		this.funcCount = 0;
	}

	/* getter & setter */
	public FuncEntry[] getTable() {
		return table;
	}

	public void setTable(FuncEntry[] table) {
		this.table = table;
	}

	public int getFuncCount() {
		return funcCount;
	}

	public void setFuncCount(int funcCount) {
		this.funcCount = funcCount;
	}

	private FuncEntry searchByName(String name) {
		if (funcCount > 0)
			for (int i = 0; i < funcCount; i++)
				if (table[i].getFuncName().equals(name))
					return table[i];
		// 空表或未查找到
		return null;
	}
}
