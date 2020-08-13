#pragma once

#include <string>

namespace os {
	std::string realpath(const std::string& relpath);
	std::string getcwd();
}