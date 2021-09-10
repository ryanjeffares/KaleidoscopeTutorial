#pragma once

namespace kaleidoscope
{
    namespace logging
    {
        namespace internal
        {
            enum LogType
            {
                Token,
                Proto,
                Value
            };

            // simple error handlers
            static void logError(const char* str, LogType type)
            {                
                switch (type)
                {
                    case LogType::Token:                        
                        fprintf(stderr, "Error of type Token: ");
                        break;
                    case LogType::Proto:
                        fprintf(stderr, "Error of type Prototype: ");
                        break;
                    case LogType::Value:
                        fprintf(stderr, "Error of type Value: ");
                        break;
                }
                fprintf(stderr, "%s\n", str);                
            }
        } // namespace private            
        
        static void logErrorToken(const char* str)
        {
            internal::logError(str, internal::LogType::Token);
        }

        static void logErrorProto(const char* str)
        {
            internal::logError(str, internal::LogType::Proto);
        }

        static void logErrorValue(const char* str)
        {
            internal::logError(str, internal::LogType::Value);
        }        
    } // namespace logging
} // namespace kaleidoscope
