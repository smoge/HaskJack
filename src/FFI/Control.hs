{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module FFI.Control where

import Control.DeepSeq -- deepseq
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.Ptr
import GHC.Generics (Generic)

{-

### Data Types
- **Parameter Types (`jackctl_param_type_t`)**: Defines types of parameters that can be controlled through the API. These include integers, unsigned integers, characters, strings, and booleans.
- **Driver Types (`jackctl_driver_type_t`)**: Specifies whether a driver is a master or a slave, affecting how it interacts within the JACK server architecture.

### Functions
- **Signal Handling**:
  - `jackctl_setup_signals`: Sets up process signal handling necessary for the server's operation.
  - `jackctl_wait_signals`: Blocks and waits for specified signals, useful in managing asynchronous events.

- **Server Management**:
  - `jackctl_server_create2`: Creates a server object with optional callbacks for device acquisition and release.
  - `jackctl_server_destroy`: Destroys a server object to free resources.
  - `jackctl_server_open`: Opens a JACK server using a specified driver.
  - `jackctl_server_start`: Starts the JACK server, enabling audio processing.
  - `jackctl_server_stop`: Stops the JACK server, halting audio processing.
  - `jackctl_server_close`: Closes the JACK server, likely freeing up resources used by the server.

- **Driver and Client Management**:
  - `jackctl_server_get_drivers_list`: Retrieves a list of available drivers.
  - `jackctl_server_get_internals_list`: Retrieves a list of internal clients.
  - `jackctl_server_load_internal`: Loads an internal client into the server.
  - `jackctl_server_unload_internal`: Unloads an internal client from the server.

- **Session and Configuration Management**:
  - Functions to add or remove drivers, switch master drivers, and handle session files.
  - Parameter management functions to get and set parameters, check constraints, and reset parameters to default values.

### Logging
- **Logging Functions**:
  - `jack_error`, `jack_info`, `jack_log`: Functions provided to log errors, information, and verbose messages, aiding in debugging and information tracking during development and runtime.

### Usage
This API is crucial for applications that need to interact directly with the JACK server for audio processing tasks, such as dynamic audio routing, processing management, and real-time audio manipulation. It provides extensive control over the server's behavior and configuration, making it a powerful tool for developers working in audio processing fields.

In summary, the JACK control API is designed to offer comprehensive control over the JACK server, allowing for detailed management of audio processing environments, making it a vital resource in advanced audio applications and research.
  -}

{-----------------------                    ------------------------------------}
-- control.h

-- | Opaque types corresponding to various JACK control structures
-- | These are used in Haskell as pointers to the underlying C structs which are not manipulated directly
-- /** opaque type for server object */
-- typedef struct jackctl_server jackctl_server_t;

-- /** opaque type for driver object */
-- typedef struct jackctl_driver jackctl_driver_t;

-- /** opaque type for internal client object */
-- typedef struct jackctl_internal jackctl_internal_t;

-- /** opaque type for parameter object */
-- typedef struct jackctl_parameter jackctl_parameter_t;

-- /** opaque type for sigmask object */
-- typedef struct jackctl_sigmask jackctl_sigmask_t;

data JackCtlServer

data JackCtlDriver

data JackCtlInternal

data JackCtlParameter

data JackCtlSigmask

type CJackCtlServer = Ptr JackCtlServer

type CJackCtlDriver = Ptr JackCtlDriver

type CJackCtlInternal = Ptr JackCtlInternal

type CJackCtlParameter = Ptr JackCtlParameter

type CJackCtlSigmask = Ptr JackCtlSigmask

-- | Enums for driver types
-- | These types distinguish between master and slave drivers within the JACK server
data JackCtlDriverType
  = -- | master driver
    JackMaster
  | -- | slave driver
    JackSlave
  deriving (Enum, Bounded, Show)

-- | Union type representing parameter values, analogous to the union in C
-- | This type allows multiple different representations for parameter values based on the parameter type
-- /** @brief Type for parameter value */
-- /* intentionally similar to jack_driver_param_value_t */
-- union jackctl_parameter_value
-- {
--     uint32_t ui;				/**< @brief member used for ::JackParamUInt */
--     int32_t i;					/**< @brief member used for ::JackParamInt */
--     char c;						/**< @brief member used for ::JackParamChar */
--     char str[JACK_PARAM_STRING_MAX + 1]; /**< @brief member used for ::JackParamString */
--     bool b;				/**< @brief member used for ::JackParamBool */
-- };
data JackCtlParameterValue = JackCtlParameterValue
  { ui  :: CUInt,
    i   :: CInt,
    c   :: CChar,
    str :: CString,
    b   :: CInt -- Boolean values are often represented as CInt in FFI, where 0 is False and not 0 is True
  }
  deriving (Generic, Show)

instance NFData JackCtlParameterValue

-- The maximum size for the string in JackCtlParameterValue
jackParamStringMax :: Int
jackParamStringMax = 127


-- * Memory Management with Storable

-- To handle the memory aspects of this union, Storable instance for JackCtlParameterValue.
-- This involves defining how to peek and poke this data type,
-- although managing a union correctly can be tricky because it involves manual memory operations:

instance Storable JackCtlParameterValue where
  sizeOf _ =
    max
      (max (sizeOf (undefined :: CUInt)) (sizeOf (undefined :: CInt)))
      (max (sizeOf (undefined :: CChar)) (jackParamStringMax + 1))
  alignment _ = alignment (undefined :: CInt)

  peek ptr = do
    u <- peekByteOff ptr 0
    i <- peekByteOff ptr 0
    c <- peekByteOff ptr 0
    strPtr <- peekByteOff ptr 0
    b <- peekByteOff ptr 0
    str <- peekCString strPtr
    pure JackCtlParameterValue {ui = u, i = i, c = c, str = str, b = b}

  poke ptr (JackCtlParameterValue u i c str b) = do
    pokeByteOff ptr 0 u
    pokeByteOff ptr 0 i
    pokeByteOff ptr 0 c
    withCString str $ \strPtr -> pokeByteOff ptr 0 strPtr
    pokeByteOff ptr 0 b

-- *Generic and NFData:
-- ***These are used for potential optimizations and deep evaluation, which can be useful for complex data types in a concurrent environment.

-- *Storable Instance:
-- *** It's crucial to manage how the data is laid out in memory since Haskell does not natively understand C unions. The poke and peek methods copy data to and from Haskell and C structures, respectively.

-- *Handling Strings:
-- *** Strings in C are pointers, special care (like `withCString`) is necessary to manage their memory outside the lifecycle of Haskell garbage collectior.

-- the management of string memory here may lead to issues if not carefully handled, especially in the context of memory allocation and deallocation across the FFI boundary. More sophisticated and safer handling might involve using ByteString or Text for string management, along with proper conversion functions.





-- | Setup process signal handling for the JACK server.
-- This function configures how the JACK server handles system signals based on the provided flags.
-- As per the JACK API, this setup is crucial for the proper operation of the server.
--
-- * @flags@ - Signal setup flags (use 0 for none, since currently no flags are defined).
-- Returns a pointer to the configured signal set, which is represented as an opaque type in Haskell.
--
-- jackctl_sigmask_t *
-- jackctl_setup_signals
--   ( unsigned int flags);
foreign import ccall unsafe "jack/control.h jackctl_setup_signals"
  c_jackctl_setup_signals :: CUInt -> IO CJackCtlSigmask

-- | Wait on a configured signal set.
-- This function blocks the calling thread until one of the signals in the specified signal set occurs.
-- It is typically used to pause the operation until a significant event or signal is received.
--
-- * @signals@ - A pointer to a signal set to wait on, configured via `c_jackctl_setup_signals`.
foreign import ccall unsafe "jack/control.h jackctl_wait_signals"
  c_jackctl_wait_signals :: CJackCtlSigmask -> IO ()

-- | Function prototypes from C with corresponding Haskell FFI bindings
-- | These bindings allow Haskell code to call C functions from the JACK control API

-- | Create a JACK server object
-- | on_device_acquire and on_device_release are callbacks for device management
foreign import ccall unsafe "jack/control.h jackctl_server_create"
  c_jackctl_server_create :: FunPtr (CString -> Bool) -> FunPtr (CString -> ()) -> IO CJackCtlServer

-- | Destroy a JACK server object
-- | This function should be called to clean up a server object created with jackctl_server_create
foreign import ccall unsafe "jack/control.h jackctl_server_destroy"
  c_jackctl_server_destroy :: CJackCtlServer -> IO ()


-- | Creates a JACK server object with optional callbacks for device handling
-- -- @param on_device_acquire - Callback before device acquisition, fails if returns false
-- -- @param on_device_release - Callback after device release
-- -- @param on_device_reservation_loop - Callback during reservation idling
-- -- @return A pointer to the server object, or NULL if creation failed
--
-- jackctl_server_t *
-- jackctl_server_create2(
--    bool (* on_device_acquire)(const char * device_name),
--    void (* on_device_release)(const char * device_name),
--    void (* on_device_reservation_loop)(void));
--
-- 1. **on_device_acquire**:
--   - **Type**: `bool (*)(const char * device_name)`
--   - **Description**: This is a callback function that is called before the server attempts to acquire an audio device. The function receives the name of the device as its parameter.
--   - **Purpose**: The callback should return `true` if the device can be successfully acquired or `false` if the device should not be used, perhaps due to it being unavailable or unsuitable. This allows you to implement custom logic to determine if a device should be used by the JACK server.
--
-- 2. **on_device_release**:
--   - **Type**: `void (*)(const char * device_name)`
--   - **Description**: This callback function is invoked after the server has finished using an audio device.
--   - **Purpose**: It allows you to execute cleanup operations or logging related to device release. This can be useful for releasing any additional resources associated with the device that were set up in the `on_device_acquire` callback.
--
-- 3. on_device_reservation_loop:
--   - Type: `void (*)(void)`
--   - This callback function is called repeatedly when the server is idling or looping, waiting for device reservations to change or finalize.
--   - It provides a hook for implementing logic during the reservation idling phase, such as checking for device state changes, performing periodic checks, or updating application status.
--
-- * Return Value
-- - **Type**: `jackctl_server_t *`
-- - **Description**: Returns a pointer to a `jackctl_server_t` structure that represents the newly created server object.
-- - **Failure**: If the server object cannot be created, possibly due to insufficient resources or initialization failures, the function returns `NULL`.
foreign import ccall unsafe "jack/control.h jackctl_server_create2"
  c_jackctl_server_create2 :: FunPtr (CString -> Bool) -> FunPtr (CString -> IO ()) -> FunPtr (IO ()) -> IO CJackCtlServer

-- | Destroys a JACK server object
-- @param server - Server object handle

{-
void
jackctl_server_destroy(
	jackctl_server_t * server);
-}
foreign import ccall unsafe "jack/control.h jackctl_server_destroy"
  c_jackctl_server_destroy :: CJackCtlServer -> IO ()

-- | Opens a JACK server using a specific driver
-- @param server - Server object handle
-- @param driver - Driver to use
-- @return Boolean status: True on success, False on failure
-- bool
-- jackctl_server_open(
--     jackctl_server_t * server,
--     jackctl_driver_t * driver);
foreign import ccall unsafe "jack/control.h jackctl_server_open"
  c_jackctl_server_open :: CJackCtlServer -> CJackCtlDriver -> IO Bool

-- | Starts the JACK server
-- @param server - Server object handle
-- @return Boolean status: True on success, False on failure
foreign import ccall unsafe "jack/control.h jackctl_server_start"
  c_jackctl_server_start :: CJackCtlServer -> IO Bool

-- | Stops the JACK server
-- @param server - Server object handle
-- @return Boolean status: True on success, False on failure
foreign import ccall unsafe "jack/control.h jackctl_server_stop"
  c_jackctl_server_stop :: CJackCtlServer -> IO Bool

-- | Closes the JACK server
-- @param server - Server object handle
-- @return Boolean status: True on success, False on failure
foreign import ccall unsafe "jack/control.h jackctl_server_close"
  c_jackctl_server_close :: CJackCtlServer -> IO Bool

-- | Retrieves a list of available drivers
-- @param server - Server object handle
-- @return A constant pointer to a list of driver handles
foreign import ccall unsafe "jack/control.h jackctl_server_get_drivers_list"
  c_jackctl_server_get_drivers_list :: CJackCtlServer -> IO JSList

-- | Retrieves a list of server parameters
-- @param server - Server object handle
-- @return A constant pointer to a list of parameter handles
foreign import ccall unsafe "jack/control.h jackctl_server_get_parameters"
  c_jackctl_server_get_parameters :: CJackCtlServer -> IO JSList

-- | Retrieves a list of available internal clients
-- @param server - Server object handle
-- @return A constant pointer to a list of internal client handles
foreign import ccall unsafe "jack/control.h jackctl_server_get_internals_list"
  c_jackctl_server_get_internals_list :: CJackCtlServer -> IO JSList

-- | Loads an internal client
-- @param server - Server object handle
-- @param internal - Internal client to load
-- @return Boolean status: True on success, False on failure
foreign import ccall unsafe "jack/control.h jackctl_server_load_internal"
  c_jackctl_server_load_internal :: CJackCtlServer -> CJackCtlInternal -> IO Bool

-- | Unloads an internal client
-- @param server - Server object handle
-- @param internal - Internal client to unload
-- @return Boolean status: True on success, False on failure
foreign import ccall unsafe "jack/control.h jackctl_server_unload_internal"
  c_jackctl_server_unload_internal :: CJackCtlServer -> CJackCtlInternal -> IO Bool
