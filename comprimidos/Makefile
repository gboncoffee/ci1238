SRC = comprimidos.hs
TARGET = comprimidos
HC = ghc

$(TARGET): $(SRC)
	$(HC) -o $@ $^

.PHONY: clean

clean:
	-rm $(TARGET)
