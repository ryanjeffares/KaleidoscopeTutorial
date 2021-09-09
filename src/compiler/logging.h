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
                const char* typeMessage;
                switch (type)
                {
                    case LogType::Token:
                        typeMessage = "Token";
                        break;
                    case LogType::Proto:
                        typeMessage = "Prototype";
                        break;
                    case LogType::Value:
                        typeMessage = "Value";
                        break;
                }
                fprintf(stderr, "Error of type %s: %s\n", typeMessage, str);                
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
