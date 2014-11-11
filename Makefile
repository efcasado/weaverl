# Author: Enrique Fernandez <efcasado@gmail.com>

REBAR          := $(shell which rebar || ./rebar)

ERL            := $(shell which erl)
ERLC           := $(shell which erlc)
ERLC_OPTS      := -pa deps/*/ebin -pz ebin "+{weaverl_aspects, [{{\"speaker\", \"choose_saying\", 1}, {after_returning, {censor, censor}}}]}"

SRC_DIR        := src
EBIN_DIR       := ebin
TEST_DIR       := test

SRC_FILES      := $(notdir $(shell find $(SRC_DIR) -name "*.erl"))
BIN_FILES      := $(patsubst %.erl,$(EBIN_DIR)/%.beam,$(SRC_FILES))
TEST_FILES     := $(notdir $(shell find $(TEST_DIR) -name "*.erl"))
BIN_TEST_FILES := $(patsubst %.erl,%.beam,$(TEST_FILES))

TEST_RULES     := $(filter %_test,$(patsubst %.erl,%,$(TEST_FILES)))

# Virtual path
VPATH = $(SRC_DIR) $(TEST_DIR)


.PHONY: $(EBIN_DIR) $(SRC_DIR) $(TEST_DIR) deps

build: deps $(EBIN_DIR) $(BIN_FILES) build-test

deps:
	$(REBAR) get-deps compile

build-test: $(BIN_TEST_FILES)

$(EBIN_DIR):
	@[ -d $(EBIN_DIR) ] || mkdir -p $(EBIN_DIR)


# Note! Everything is re-compiled if weaverl.erl has changed.
# This is done to speed-up Weaverl's development. Everything in
# this project revolves around Weaverl's parse transform, which
# is, in fact, defined in the weaverl.erl module.

# This rule is used to compile source files.
ebin/%.beam: %.erl weaverl.erl
	@echo "Compiling module $(notdir $<)"
	@$(ERLC) $(ERLC_OPTS) -o ebin $<

# This rule is used to compile test files.
%.beam: %.erl weaverl.erl
	@echo "Compiling test $(notdir $<)"
	@$(ERLC) $(ERLC_OPTS) -o $(dir $<) $<

test: $(TEST_RULES)

%_test: $(TEST_DIR)/%_test.beam
	@$(ERL) -pa $(EBIN_DIR) -pa $(TEST_DIR) -noshell \
		-eval "$@:run()"                             \
		-eval 'halt()'

clean:
	$(shell find . -name "*.beam" -delete)

info:
	@echo "# =========================="
	@echo "#  Variables                "
	@echo "# =========================="
	@echo "   SRC_FILES = "$(SRC_FILES)
	@echo "   INC_FILES = "$(INC_FILES)
	@echo "   BIN_FILES = "$(BIN_FILES)
	@echo "   VPATH     = "$(VPATH)
	@echo
	@echo "# =========================="
	@echo "#  Erlang compiler          "
	@echo "# =========================="
	@echo "   ERLC      = "$(ERLC)
	@echo "   ERLC_OPTS = "$(ERLC_OPTS)
