#pragma once

namespace kaleidoscope
{
    namespace logging
    {
        namespace detail
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
        } // namespace detail            
        
        static void logErrorToken(const char* str)
        {
            detail::logError(str, detail::LogType::Token);
        }

        static void logErrorProto(const char* str)
        {
            detail::logError(str, detail::LogType::Proto);
        }

        static void logErrorValue(const char* str)
        {
            detail::logError(str, detail::LogType::Value);
        }        
    } // namespace logging
} // namespace kaleidoscope
