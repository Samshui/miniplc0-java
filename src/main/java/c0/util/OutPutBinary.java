package c0.util;

import c0.Entry.FuncEntry;
import c0.Entry.SymbolEntry;
import c0.instruction.Instruction;
import c0.table.Table;
import c0.tokenizer.TokenType;

import java.util.ArrayList;
import java.util.List;

public class OutPutBinary {
	Table table;
	List<Byte> output;

	int magic = 0x72303b3e;
	int version = 0x00000001;

	public OutPutBinary(Table table) {
		this.table = table;
		output = new ArrayList<>();
	}

	public List<Byte> generate() {
		// 魔数
		List<Byte> magic = int2bytes(4, this.magic);
		output.addAll(magic);

		// 版本号
		List<Byte> version = int2bytes(4, this.version);
		output.addAll(version);

		// 全局变量表
		List<SymbolEntry> globals = table.getGlobalSymTable();

		List<Byte> globalCount = int2bytes(4, globals.size());
		output.addAll(globalCount);

		for (int i = 0; i < globals.size(); i++) {
			SymbolEntry oneGlobalSymbol = globals.get(i);

			List<Byte> globalIsConst = int2bytes(1, oneGlobalSymbol.isConst());
			output.addAll(globalIsConst);

			List<Byte> globalValue = getValueByte(oneGlobalSymbol);

			List<Byte> globalValueCount = int2bytes(4, globalValue.size());
			output.addAll(globalValueCount);

			output.addAll(globalValue);
		}

		// 函数列表
		List<FuncEntry> functionTables = table.getFuncTable();

		List<Byte> functions_count = int2bytes(4, functionTables.size());
		output.addAll(functions_count);

		for (int i = 0; i < functionTables.size(); i++) {
			FuncEntry oneFunction = functionTables.get(i);

			// 函数名
			List<Byte> name = int2bytes(4, table.getFuncID(oneFunction.getFuncName()));
			output.addAll(name);

			// 返回值
			List<Byte> retSlots = int2bytes(4, (oneFunction.getFuncType() == TokenType.VOID_TY) ? 0 : 1);
			output.addAll(retSlots);

			// 参数
			List<Byte> paramsSlots = int2bytes(4, oneFunction.getParamSlotCount());
			output.addAll(paramsSlots);

			// 局部变量
			List<Byte> locSlots = int2bytes(4, oneFunction.getVarSlotCount());
			output.addAll(locSlots);

			// 函数体指令
			List<Instruction> instructions = oneFunction.getInstructions();
			List<Byte> bodyCount = int2bytes(4, instructions.size());
			output.addAll(bodyCount);

			// 指令集
			for (Instruction instruction : instructions) {
				// type
				List<Byte> type = int2bytes(1, instruction.getType());
				output.addAll(type);

				if (instruction.getLen() == 2) {
					List<Byte> x;
					if (instruction.getType() == 1)
						x = long2bytes(8, instruction.getOff());
					else x = long2bytes(4, instruction.getOff());
					output.addAll(x);
				}
			}
		}
		return output;
	}

	private List<Byte> getValueByte(SymbolEntry globalSymbol) {
		List<Byte> bytes = new ArrayList<>();

		if (globalSymbol.getSymbolType() == SymbolType.FUNC || globalSymbol.getSymbolType() == SymbolType.STRING)
			bytes = String2bytes(globalSymbol.getName());
		else {
			if (globalSymbol.getType() == TokenType.INT_TY)
				bytes = long2bytes(8, 0);
		}
		return bytes;
	}

	private List<Byte> Char2bytes(char value) {
		List<Byte> AB = new ArrayList<>();
		AB.add((byte) (value & 0xff));
		return AB;
	}

	private List<Byte> String2bytes(String valueString) {
//		List<Byte> AB = new ArrayList<>();
//		for (int i = 0; i < valueString.length(); i++) {
//			char ch = valueString.charAt(i);
//			if (ch != '\\')
//				AB.add((byte) (ch & 0xff));
//			else {
//				i++;
//				ch = valueString.charAt(i);
//				if (ch == '\\')
//					AB.add((byte) ('\\' & 0xff));
//				else if (ch == '\"')
//					AB.add((byte) ('\"' & 0xff));
//				else if (ch == '\'')
//					AB.add((byte) ('\'' & 0xff));
//				else if (ch == 'n')
//					AB.add((byte) ('\n' & 0xff));
//				else if (ch == 'r')
//					AB.add((byte) ('\r' & 0xff));
//				else if (ch == 't')
//					AB.add((byte) ('\t' & 0xff));
//			}
//		}
//		return AB;
		List<Byte> AB = new ArrayList<>();
		for (int i = 0; i < valueString.length(); i++) {
			char ch = valueString.charAt(i);
			AB.add((byte) (ch & 0xff));
		}
		return AB;
	}

	private List<Byte> long2bytes(int length, long target) {
		ArrayList<Byte> bytes = new ArrayList<>();
		int start = 8 * (length - 1);
		for (int i = 0; i < length; i++) {
			bytes.add((byte) ((target >> (start - i * 8)) & 0xFF));
		}
		return bytes;
	}

	private ArrayList<Byte> int2bytes(int length, int target) {
		ArrayList<Byte> bytes = new ArrayList<>();
		int start = 8 * (length - 1);
		for (int i = 0; i < length; i++) {
			bytes.add((byte) ((target >> (start - i * 8)) & 0xFF));
		}
		return bytes;
	}
}
