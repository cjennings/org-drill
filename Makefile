# Makefile for org-drill
#
# Usage:
#   make help              - Show this help message
#   make test              - Run all tests (unit + robot)
#   make test-unit         - Run ERT unit tests only
#   make test-file FILE=org-drill-test.el  - Run specific test file
#   make test-name TEST=load-test          - Run tests matching pattern
#   make robot             - Run basic robot tests
#   make robot-all         - Run all robot tests
#   make docker-test       - Run tests in Docker (multiple Emacs versions)
#   make install           - Install dependencies via Cask
#   make clean             - Remove generated files

# Emacs binary to use (override with: make EMACS=emacs29 test)
EMACS ?= emacs
# Check for Cask in PATH or common installation location
CASK ?= $(shell command -v cask 2>/dev/null || echo "$(HOME)/.cask/bin/cask")

# Include local overrides if present
-include makefile-local

ifdef EMACS
EMACS_ENV=EMACS=$(EMACS)
endif

# Test directories and files
TEST_DIR = tests
UNIT_TESTS = $(filter-out $(TEST_DIR)/%-integration-test.el, $(wildcard $(TEST_DIR)/*-test.el))
INTEGRATION_TESTS = $(wildcard $(TEST_DIR)/*-integration-test.el)
ALL_TESTS = $(UNIT_TESTS) $(INTEGRATION_TESTS)

# Emacs batch flags
EMACS_BATCH = $(EMACS) --batch --no-site-file --no-site-lisp

# Docker configuration
DOCKER_TAG=26

.PHONY: help test test-all test-unit test-integration test-file test-name install build clean clean-elc
.PHONY: robot robot-all robot-basic robot-leitner robot-all-card robot-spanish robot-explainer
.PHONY: docker-test test-cp test-git

# Default target
help:
	@echo "org-drill Test Targets:"
	@echo ""
	@echo "Primary Targets:"
	@echo "  make help              - Show this help message"
	@echo "  make test              - Run all tests (unit + robot)"
	@echo "  make test-unit         - Run unit tests only ($(words $(UNIT_TESTS)) files)"
	@echo "  make test-integration  - Run integration tests only ($(words $(INTEGRATION_TESTS)) files)"
	@echo "  make robot             - Run basic robot tests"
	@echo "  make robot-all         - Run all robot tests"
	@echo ""
	@echo "Selective Testing:"
	@echo "  make test-file FILE=<filename>  - Run specific test file"
	@echo "  make test-name TEST=<pattern>   - Run tests matching pattern"
	@echo ""
	@echo "Advanced Targets:"
	@echo "  make install           - Install dependencies via Cask"
	@echo "  make build             - Build package via Cask"
	@echo "  make docker-test       - Run tests in Docker (multiple Emacs versions)"
	@echo "  make clean             - Remove generated files"
	@echo "  make clean-elc         - Remove compiled .elc files"
	@echo ""
	@echo "Examples:"
	@echo "  make test-file FILE=org-drill-test.el"
	@echo "  make test-name TEST=load-test"
	@echo "  make test-name TEST='find-*'"
	@echo "  make EMACS=emacs29 test   # Use specific Emacs version"
	@echo ""
	@echo "Robot Test Targets:"
	@echo "  make robot-basic       - Run basic robot test"
	@echo "  make robot-leitner     - Run Leitner algorithm test"
	@echo "  make robot-all-card    - Run all-card robot test"
	@echo "  make robot-spanish     - Run Spanish vocabulary test"
	@echo "  make robot-explainer   - Run explainer robot test"

# Default: run all tests
all: robot test-unit

# Install dependencies
install:
	@if ! command -v $(CASK) >/dev/null 2>&1; then \
		echo "[✗] Cask not found. Please install Cask first:"; \
		echo "    https://github.com/cask/cask"; \
		echo "    Or: curl -fsSL https://raw.githubusercontent.com/cask/cask/master/go | python"; \
		exit 1; \
	fi
	@echo "[i] Installing dependencies via Cask..."
	@$(EMACS_ENV) $(CASK) install
	@echo "[✓] Dependencies installed"

# Build package
build:
	$(EMACS_ENV) $(CASK) build

# Run all tests
test: robot test-unit

# Full test suite
test-all: robot-all test-unit test-integration

# Run unit tests only
test-unit: install
	@echo "[i] Running unit tests ($(words $(UNIT_TESTS)) files)..."
	@failed=0; \
	for test in $(UNIT_TESTS); do \
		echo "  Testing $$test..."; \
		$(EMACS_ENV) $(CASK) emacs --batch -q \
			-l ert \
			-l assess \
			-l org-drill.el \
			-l $$test \
			-f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
	done; \
	if [ $$failed -eq 0 ]; then \
		echo "[✓] All unit tests passed"; \
	else \
		echo "[✗] $$failed unit test file(s) failed"; \
		exit 1; \
	fi

# Run integration tests only
test-integration: install
	@echo "[i] Running integration tests ($(words $(INTEGRATION_TESTS)) files)..."
	@if [ $(words $(INTEGRATION_TESTS)) -eq 0 ]; then \
		echo "[i] No integration tests found"; \
	else \
		failed=0; \
		for test in $(INTEGRATION_TESTS); do \
			echo "  Testing $$test..."; \
			$(EMACS_ENV) $(CASK) emacs --batch -q \
				-l ert \
				-l assess \
				-l org-drill.el \
				-l $$test \
				-f ert-run-tests-batch-and-exit || failed=$$((failed + 1)); \
		done; \
		if [ $$failed -eq 0 ]; then \
			echo "[✓] All integration tests passed"; \
		else \
			echo "[✗] $$failed integration test file(s) failed"; \
			exit 1; \
		fi; \
	fi

# Run specific test file
# Usage: make test-file FILE=org-drill-test.el
test-file: install
ifndef FILE
	@echo "[✗] Error: FILE parameter required"
	@echo "Usage: make test-file FILE=org-drill-test.el"
	@exit 1
endif
	@echo "[i] Running tests in $(FILE)..."
	@$(EMACS_ENV) $(CASK) emacs --batch -q \
		-l ert \
		-l assess \
		-l org-drill.el \
		-l $(TEST_DIR)/$(FILE) \
		-f ert-run-tests-batch-and-exit
	@echo "[✓] Tests in $(FILE) complete"

# Run specific test by name/pattern
# Usage: make test-name TEST=load-test
#        make test-name TEST="find-*"
test-name: install
ifndef TEST
	@echo "[✗] Error: TEST parameter required"
	@echo "Usage: make test-name TEST=load-test"
	@echo "       make test-name TEST='find-*'"
	@exit 1
endif
	@echo "[i] Running tests matching pattern: $(TEST)..."
	@$(EMACS_ENV) $(CASK) emacs --batch -q \
		-l ert \
		-l assess \
		-l org-drill.el \
		$(foreach test,$(ALL_TESTS),-l $(test)) \
		--eval '(ert-run-tests-batch-and-exit "$(TEST)")'
	@echo "[✓] Tests matching '$(TEST)' complete"

#
# Robot Tests (Automated UI Tests)
#

# Basic robot test (default for quick testing)
robot: robot-basic

# All robot tests
robot-all: robot-basic robot-leitner robot-all-card robot-spanish

# Individual robot tests
robot-basic: clean-elc
	@echo "[i] Running basic robot test..."
	@$(EMACS_ENV) ./robot/basic-run.sh $(SMALL)
	@echo "[✓] Basic robot test complete"

robot-leitner: clean-elc
	@echo "[i] Running Leitner robot test..."
	@$(EMACS_ENV) ./robot/leitner-run.sh $(SMALL)
	@echo "[✓] Leitner robot test complete"

robot-all-card: clean-elc
	@echo "[i] Running all-card robot test..."
	@$(EMACS_ENV) ./robot/all-card-run.sh $(SMALL)
	@echo "[✓] All-card robot test complete"

robot-spanish: clean-elc
	@echo "[i] Running Spanish robot test..."
	@$(EMACS_ENV) ./robot/spanish-run.sh $(SMALL)
	@echo "[✓] Spanish robot test complete"

robot-explainer: clean-elc
	@echo "[i] Running explainer robot test..."
	@$(EMACS_ENV) ./robot/explainer-run.sh $(SMALL)
	@echo "[✓] Explainer robot test complete"

#
# Docker Testing (Multiple Emacs Versions)
#

test-cp:
	docker run -it --rm --name docker-cp \
		-v $(PWD):/usr/src/app \
		-w /usr/src/app \
		--entrypoint=/bin/bash \
		silex/emacs:$(DOCKER_TAG)-dev ./test-by-cp

test-git:
	docker run -it --rm --name docker-git \
		-v $(PWD):/usr/src/app \
		-w /usr/src/app \
		--entrypoint=/bin/bash \
		silex/emacs:$(DOCKER_TAG)-dev ./test-from-git

docker-test:
	@echo "[i] Running Docker tests across multiple Emacs versions..."
	@$(MAKE) test-git DOCKER_TAG=27.2
	@$(MAKE) test-cp DOCKER_TAG=27.2
	@$(MAKE) test-git DOCKER_TAG=26.3
	@$(MAKE) test-cp DOCKER_TAG=26.3
	@echo "[✓] Docker tests complete"

#
# Cleanup
#

clean-elc:
	@echo "[i] Cleaning compiled .elc files..."
	@find . -name "*.elc" -delete
	@echo "[✓] Clean .elc complete"

clean:
	@echo "[i] Cleaning generated files..."
	@find . -name "*.elc" -delete
	@find $(TEST_DIR) -name "*-test-*" -type f -delete
	@echo "[✓] Clean complete"
