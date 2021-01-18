package c0.util;

import c0.instruction.Instruction;

import java.util.ArrayList;
import java.util.List;

public class FileFill {
	public List<String> files = new ArrayList<>();
	private List<Instruction> instructions = new ArrayList<>();

	public FileFill() {
		this.files.addAll(
				List.of(
						"/tests/6-break-continue/ac1-fastpow-v2.c0",
						"/tests/6-break-continue/ac2-guess-v3.c0",
						"/tests/6-break-continue/ac3.c0"));
	}
}
