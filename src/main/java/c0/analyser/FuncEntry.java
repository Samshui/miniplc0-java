package c0.analyser;

public class FuncEntry {
	private String funcName;
	private int symbolIndex;
	private int blockLayer;

	public FuncEntry(String funcName, int symbolIndex, int blockLayer) {
		this.funcName = funcName;
		this.symbolIndex = symbolIndex;
		this.blockLayer = blockLayer;
	}

	// getter & setter
	public String getFuncName() {
		return funcName;
	}

	public void setFuncName(String funcName) {
		this.funcName = funcName;
	}

	public int getSymbolIndex() {
		return symbolIndex;
	}

	public void setSymbolIndex(int symbolIndex) {
		this.symbolIndex = symbolIndex;
	}

	public int getBlockLayer() {
		return blockLayer;
	}

	public void setBlockLayer(int blockLayer) {
		this.blockLayer = blockLayer;
	}
}
