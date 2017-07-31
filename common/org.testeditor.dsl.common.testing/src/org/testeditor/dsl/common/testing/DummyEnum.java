package org.testeditor.dsl.common.testing;

public enum DummyEnum {
	enum_a("DUMMY_ENUM_A"),
	enum_b("DUMMY_ENUM_B");
	
	String stringValue;
	
	DummyEnum(String stringValue) {
		this.stringValue = stringValue;
	}
	
 }
