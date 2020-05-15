#include "Os.h"

#ifndef WIN32
#include <linux/limits.h>
#include <unistd.h>
#else
#include <Windows.h>
#include <direct.h>
#include < Pathcch.h >
#endif

namespace os {
	std::string realpath(const std::string& relpath) {
#ifndef WIN32
		char buffer[PATH_MAX];
    ::realpath(relpath.c_str(), buffer);
		return std::string(buffer);
#else
		char buffer[4096];
		GetFullPathNameA(relpath.c_str(), 4096, buffer, nullptr);
		return std::string(buffer);
#endif
	}


	std::string getcwd() {
#ifndef WIN32
		char buffer[PATH_MAX];
    ::getcwd(buffer, PATH_MAX);
		return std::string(buffer);
#else
		char buffer[4096];
		 GetModuleFileNameA(
			NULL,
			buffer,
			4096
		);
		std::string path = buffer;
		size_t p = path.rfind('\\');
		if (p != std::string::npos) {
			path = path.substr(0, p);
		}
		return path;
#endif

	}
}
