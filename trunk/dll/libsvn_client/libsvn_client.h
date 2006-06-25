// The following ifdef block is the standard way of creating macros which make exporting 
// from a DLL simpler. All files within this DLL are compiled with the LIBSVN_CLIENT_EXPORTS
// symbol defined on the command line. this symbol should not be defined on any project
// that uses this DLL. This way any other project whose source files include this file see 
// LIBSVN_CLIENT_API functions as being imported from a DLL, whereas this DLL sees symbols
// defined with this macro as being exported.
#ifdef LIBSVN_CLIENT_EXPORTS
#define LIBSVN_CLIENT_API __declspec(dllexport)
#else
#define LIBSVN_CLIENT_API __declspec(dllimport)
#endif
/*
// This class is exported from the libsvn_client.dll
class LIBSVN_CLIENT_API Clibsvn_client {
public:
	Clibsvn_client(void);
	// TODO: add your methods here.
};

extern LIBSVN_CLIENT_API int nlibsvn_client;

LIBSVN_CLIENT_API int fnlibsvn_client(void);
*/