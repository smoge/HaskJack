{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FFI.Jack where

import Control.Monad (void)
import Data.Bits (Bits, FiniteBits, bit, complement, (.&.), (.|.))
import Data.Word (Word32)
import Foreign
import Foreign.C
import Foreign.C.Types
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Ptr
import qualified GHC.IO.Handle as H

-- #include <Common.h>

{-
  jack_nframes_t: Defined in C as uint32_t.
    Haskell Equivalent: Should be CUInt,

  jack_time_t: Defined in C as uint64_t.
    Haskell Equivalent: Should be CUInt64.

jack_uuid_t: Defined in C as uint64_t.
    Haskell Equivalent: Should also be CUInt64.

jack_shmsize_t: Defined in C as int32_t.
    Haskell Equivalent: Should be CInt.

jack_port_id_t and jack_port_type_id_t: Both defined in C as uint32_t.
    Haskell Equivalent: Both should be CUInt.

jack_options_t and jack_status_t: These are enums in C but are typically handled as CUInt in Haskell because enums are essentially integers in C.

jack_latency_callback_mode_t: An enum in C.
    Haskell Equivalent: Should be handled as CInt since enums map to integers.

For the opaque structures like jack_client_t and jack_port_t that are handled via pointers:

    C:
      typedef struct _jack_client jack_client_t; and typedef struct _jack_port jack_port_t;

    Haskell Equivalent:
        These should be represented as pointers to undefined structures, i.e.,
        Ptr JackClient and Ptr JackPort, where JackClient and JackPort are dummy types in Haskell,
        defined solely for type safety.

-}

newtype PortName = PortName {getPortName :: String}

data CJackClient

data CJackPort

type JackClient = Ptr CJackClient

type JackPort = Ptr CJackPort

type JackPortId = CUInt

type JackNFrames = CUInt

type JackTime = CUInt

type JackLatencyCallbackMode = CInt

type JackLatencyRange = Ptr CUInt -- Assuming latency range is stored in an array of two CUInts

type OpenOptionSet = JackOptions

type JackStatusSet = JackStatus

-- Assuming pthread_t is equivalent to unsigned long on the platform
newtype JackNativeThread = JackNativeThread CULong
  deriving (Eq, Show)

newtype JackOptions = JackOptions CUInt
  deriving (Eq, Ord, Show, Bits, FiniteBits, Num)

-- Constants for JackOptions
jackNullOption, jackNoStartServer, jackUseExactName, jackServerName, jackLoadName, jackLoadInit, jackSessionID :: JackOptions
jackNullOption = JackOptions 0x00
jackNoStartServer = JackOptions 0x01
jackUseExactName = JackOptions 0x02
jackServerName = JackOptions 0x04
jackLoadName = JackOptions 0x08
jackLoadInit = JackOptions 0x10
jackSessionID = JackOptions 0x20

newtype JackStatus = JackStatus CUInt
  deriving (Enum, Eq, Ord, Show, Bits, FiniteBits)

-- Constants for each JackStatus
jackFailure, jackInvalidOption, jackNameNotUnique, jackServerStarted, jackServerFailed, jackServerError, jackNoSuchClient, jackLoadFailure, jackInitFailure, jackShmFailure, jackVersionError, jackBackendError, jackClientZombie :: JackStatus
jackFailure = JackStatus 0x01
jackInvalidOption = JackStatus 0x02
jackNameNotUnique = JackStatus 0x04
jackServerStarted = JackStatus 0x08
jackServerFailed = JackStatus 0x10
jackServerError = JackStatus 0x20
jackNoSuchClient = JackStatus 0x40
jackLoadFailure = JackStatus 0x80
jackInitFailure = JackStatus 0x100
jackShmFailure = JackStatus 0x200
jackVersionError = JackStatus 0x400
jackBackendError = JackStatus 0x800
jackClientZombie = JackStatus 0x1000

{-
JackNFrames, JackTime, JackLatencyCallbackMode, etc., are correctly aligned with
the corresponding C types (uint32_t, uint64_t, and int32_t).
This is crucial for ensuring that the Haskell FFI can correctly
handle data passed to and from C functions.

## Opaque Pointer Types:

 Ptr CJackClient and Ptr CJackPort for jack_client_t and jack_port_t

 These types are defined as opaque pointers in C, so they should also be treated as such in Haskell to prevent unsafe access

## Opaque Pointer Types:

    Ptr CJackClient and Ptr CJackPort for jack_client_t and jack_port_t

    These types are defined as opaque pointers in C,
    so they should also be treated as such in Haskell to prevent unsafe access

-}

-- Example: combining multiple options
exampleOptions :: OpenOptionSet
exampleOptions = jackNoStartServer .|. jackUseExactName .|. jackSessionID

-- Example: combining multiple statuses
exampleStatuses :: JackStatusSet
exampleStatuses = jackFailure .|. jackServerFailed .|. jackClientZombie

-- Combine multiple JackStatus flags into a single StatusSet
combineStatuses :: [JackStatus] -> JackStatusSet
combineStatuses = foldr (.|.) (JackStatus 0)

unJackOptions :: JackOptions -> CUInt
unJackOptions (JackOptions opts) = opts

unJackStatus :: JackStatus -> CUInt
unJackStatus (JackStatus sts) = sts

-- Combine multiple JackOptions into a single CUInt value
combineOptions :: [JackOptions] -> CUInt
combineOptions = foldr ((.|.) . optionToCUInt) 0x00

-- -- Example of using JackOptions
exampleOptions2 :: CUInt
exampleOptions2 = combineOptions [JackNoStartServer, JackUseExactName, JackSessionID]


{- | Opens an external client session with a JACK server. More powerful than jack_client_new().
-- | Unless forbidden by JackUseExactName, the server may modify the name to create a unique variant.
-- | Options are formed by OR-ing together JackOptions bits.
-- | If status is non-NULL, JACK returns information from the open operation.

jack_client_t * jack_client_open (const char *client_name,
                                  jack_options_t options,
                                  jack_status_t *status, ...) JACK_OPTIONAL_WEAK_EXPORT;
-}
foreign import capi unsafe "jack/jack.h jack_client_open"
  c_jack_client_open :: Ptr CString -> OpenOptionSet -> Ptr JackStatus -> IO JackClient


-- | Disconnects an external client from a JACK server.
-- | This function directly calls the C function 'jack_client_close' to disconnect the client.
-- | It is marked as 'unsafe' indicating that it might invoke GC-unsafe operations or block indefinitely.
-- | The function returns 0 on success, otherwise a non-zero error code indicating the type of error.
-- int jack_client_close (jack_client_t *client) JACK_OPTIONAL_WEAK_EXPORT;

foreign import capi unsafe "jack/jack.h jack_client_close"
  c_jack_client_close :: JackClient -> IO CInt


-- | This is a pointer to the 'jack_client_close' function from the JACK library.
-- | It doesn't call the function directly but provides a Haskell representation of the function pointer.
-- | This pointer can be used with 'newForeignPtr' to create a foreign pointer that automatically
-- | calls 'jack_client_close' when the Haskell garbage collector decides to collect the resource.
-- | This is useful for automatic resource management in Haskell, ensuring the JACK client is closed
-- | when no longer needed, without manually managing the close operation in every use case.
foreign import capi "jack/jack.h &jack_client_close"
  p_jack_client_close :: FunPtr (JackClient -> IO ())

{-

1. **Direct Call vs. Function Pointer:**
   - `c_jack_client_close`: Directly calls the C function to close a JACK client. It's used when you want to explicitly control when to close the client in your Haskell code.
   - `p_jack_client_close`: Represents a function pointer to the C function, used for integrating with Haskell's garbage collector via `ForeignPtr`. This is helpful for automating the cleanup process.

2. **Usage Context:**
   - `c_jack_client_close` is used in situations where you want immediate, manual control over closing the client, typically where error handling and timing of the close operation are critical.
   - `p_jack_client_close` is used when you want Haskell's runtime to manage the resource, cleaning up automatically when the associated `ForeignPtr` is garbage collected, reducing the risk of resource leaks.

3. **Safety and Resource Management:**
   - The `unsafe` keyword in `c_jack_client_close` implies potential for blocking or unsafe operations with respect to Haskell’s runtime, which needs careful handling to avoid disrupting Haskell's lightweight threading.
   - Using `p_jack_client_close` with `ForeignPtr` aligns with Haskell's approach to resource management, where the runtime handles resources safely and efficiently, abstracting much of the manual management away from the developer.

  -}




-- | Activate a JACK client.
-- Activates the client on the JACK server, allowing it to begin processing audio.
-- This operation connects the client to the JACK graph and allows it to process audio data.
--
-- * @client@ - A pointer to the JackClient structure representing the client to be activated.
--
-- Returns 0 on success, otherwise returns a non-zero error code that indicates the type of error.
-- int jack_activate (jack_client_t *client) JACK_OPTIONAL_WEAK_EXPORT;
foreign import capi unsafe "jack/jack.h jack_activate"
  c_jack_activate :: JackClient -> IO CInt

-- | Deactivate a JACK client.
-- This function removes the client from the JACK server's process graph, effectively stopping it from
-- processing audio. It also disconnects all ports owned by this client as inactive clients cannot
-- have active connections.
--
-- * @client@ - A pointer to the JackClient structure representing the client to be deactivated.
--
-- Returns 0 on success, otherwise returns a non-zero error code that indicates the type of error.
-- int jack_deactivate (jack_client_t *client) JACK_OPTIONAL_WEAK_EXPORT;
foreign import capi unsafe "jack/jack.h jack_deactivate"
  c_jack_deactivate :: JackClient -> IO CInt




-- | Retrieve the process ID of a JACK client based on its name.
-- This function looks up a JACK client by name and returns the UNIX process ID associated with it.
-- If the client is not found or the PID is not available, it returns 0.
--
-- * @name@ - A null-terminated string containing the name of the client.
--
-- Returns the process ID of the client. If not available, 0 is returned.
-- int jack_get_client_pid (const char *name) JACK_OPTIONAL_WEAK_EXPORT;

foreign import capi unsafe "jack/jack.h jack_get_client_pid"
  c_jack_get_client_pid :: CString -> IO CInt


-- | Get the pthread ID of the thread running the JACK client's real-time code.
-- This ID can be used to perform thread-specific operations in a system that supports pthreads.
--
-- * @client@ - A pointer to the JackClient structure representing the client whose thread ID is being queried.
--
-- Returns the pthread ID of the thread running the client's real-time code.
-- jack_native_thread_t jack_client_thread_id (jack_client_t *client) JACK_OPTIONAL_WEAK_EXPORT;

foreign import capi unsafe "jack/jack.h jack_client_thread_id"
  c_jack_client_thread_id :: JackClient -> IO JackNativeThread


-- | Returns the sample rate of the JACK server.
foreign import capi unsafe "jack/jack.h jack_get_sample_rate"
  c_jack_get_sample_rate :: JackClient -> IO CUInt

-- | Returns the actual client name, useful when JackUseExactName is not specified.
--
-- @return pointer to actual client name.  This is useful when @ref
-- JackUseExactName is not specified on open and @ref
-- JackNameNotUnique status was returned.  In that case, the actual
-- name will differ from the @a client_name requested.
--
-- char * jack_get_client_name (jack_client_t *client) JACK_OPTIONAL_WEAK_EXPORT;

foreign import capi unsafe "jack/jack.h jack_get_client_name"
  c_jack_get_client_name :: JackClient -> IO CString

-- | Gets the current buffer size.
foreign import capi unsafe "jack/jack.h jack_get_buffer_size"
  c_jack_get_buffer_size :: JackClient -> IO CUInt

-- | Returns the pthread ID of the thread running the JACK client side real-time code.
foreign import capi "jack/jack.h jack_client_thread_id"
  c_jack_client_thread_id :: JackClient -> IO JackNativeThread

-- | Checks if the JACK subsystem is running with -R (--realtime).
-- | @return 1 if JACK is running realtime, 0 otherwise
foreign import capi "jack/jack.h jack_is_realtime"
  c_jack_is_realtime :: JackClient -> IO CInt

-- | Create a new port for the client.
-- | Ports may be connected in various ways. Each port has a short name and a full name.
-- | The full name is the client name concatenated with a colon (:) followed by its short name.
-- | The port_name must be unique among all ports owned by this client.
-- | If the name is not unique, the registration will fail.
-- | @return jack_port_t pointer on success, otherwise NULL.
foreign import capi "jack/jack.h jack_port_register"
  c_jack_port_register :: JackClient -> CString -> CString -> CULong -> CULong -> IO JackPort

-- | Remove the port from the client, disconnecting any existing connections.
-- | @return 0 on success, otherwise a non-zero error code
foreign import capi "jack/jack.h jack_port_unregister"
  c_jack_port_unregister :: JackClient -> JackPort -> IO CInt

-- | Returns the port type id.
foreign import capi "jack/jack.h jack_port_type_id"
  c_jackPortTypeId :: JackPort -> IO JackPortId

-- | Checks if the JackPort belongs to the JackClient.
-- | @return TRUE if the jack_port_t belongs to the jack_client_t.
foreign import capi "jack/jack.h jack_port_is_mine"
  c_jackPortIsMine :: JackClient -> JackPort -> IO CInt

-- | Returns number of connections to or from a port.
-- | The calling client must own the port.
-- | @return number of connections to or from @a port.
foreign import capi "jack/jack.h jack_port_connected"
  c_jackPortConnected :: JackPort -> IO CInt

-- | Checks if the locally-owned port is directly connected to the port_name.
-- | @see jack_port_name_size()
-- | @return TRUE if the locally-owned @a port is @b directly connected to the @a port_name.
foreign import capi "jack/jack.h jack_port_connected_to"
  c_jackPortConnectedTo :: JackPort -> CString -> IO CInt

-- | Returns a null-terminated array of full port names to which the port is connected. If none, returns NULL.
-- | The caller is responsible for calling jack_free() on any non-NULL returned
-- value.
-- | @param port locally owned jack_port_t pointer.
-- | @see jack_port_name_size(), jack_port_get_all_connections()
-- | @return a null-terminated array of full port names to which the @a
-- | port is connected. If none, returns NULL.
foreign import capi "jack/jack.h jack_port_get_connections"
  c_jackPortGetConnections :: JackPort -> IO (Ptr CString)

-- | Returns a null-terminated array of full port names to which the port is
-- connected. If none, returns NULL.
-- | This differs from jack_port_get_connections() in two important respects:
-- |     1) You may not call this function from code that is executed in response to a JACK event. For example,
-- |        you cannot use it in a GraphReordered handler.
-- |     2) You need not be the owner of the port to get information about its connections.
-- | @see jack_port_name_size()
-- | @return a null-terminated array of full port names to which the @a
-- | port is connected. If none, returns NULL.
foreign import capi "jack/jack.h jack_port_get_all_connections"
  c_jackPortGetAllConnections :: JackClient -> JackPort -> IO (Ptr CString)

-- Function to modify a port's short name. It may NOT be called from a callback handling a server event.
-- If the resulting full name (including the "client_name:" prefix) is longer than
-- jack_port_name_size(), it will be truncated.
-- This differs from jack_port_set_name() by triggering PortRename notifications to
-- clients that have registered a port rename handler.
-- @return 0 on success, otherwise a non-zero error code.
foreign import capi "jack/jack.h jack_port_rename"
  c_jackPortRename :: JackClient -> JackPort -> CString -> IO CInt

-- Function to set @alias as an alias for @port. It may be called at any time.
-- If the alias is longer than jack_port_name_size(), it will be truncated.
-- After a successful call, and until JACK exits or jack_port_unset_alias() is called, @alias may be used as an alternate name for the port.
-- Ports can have up to two aliases - if both are already set, this function will return an error.
-- @return 0 on success, otherwise a non-zero error code.
foreign import capi "jack/jack.h jack_port_set_alias"
  c_jackPortSetAlias :: JackPort -> CString -> IO CInt

-- Function to remove @alias as an alias for @port. It may be called at any time.
-- After a successful call, @alias can no longer be used as an alternate name for the port.
-- @return 0 on success, otherwise a non-zero error code.
foreign import capi "jack/jack.h  jack_port_unset_alias"
  c_jackPortUnsetAlias :: JackPort -> CString -> IO CInt

-- Function to get any aliases known for @port.
-- @return the number of aliases discovered for the port
foreign import capi "jack/jack.h  jack_port_get_aliases"
  c_jackPortGetAliases :: JackPort -> Ptr CString -> IO CInt

-- Helper function to convert a list of CStrings to Haskell list of Strings
cStringsToList :: Int -> Ptr CString -> IO [String]
cStringsToList count ptr = do
  cstrs <- peekArray count ptr
  mapM peekCString cstrs

-- -- Wrapper function to get aliases
-- getPortAliases :: JackPort -> IO [String]
-- getPortAliases (JackPort portPtr) = allocaArray 2 $ \aliasesPtr -> do
--   count <- c_jackPortGetAliases portPtr aliasesPtr
--   cStringsToList (fromIntegral count) aliasesPtr

-- ! Note: These functions will likely require additional error handling and memory management.

-- Function to check if input monitoring has been requested for a port.
-- @return TRUE if input monitoring has been requested for @a port.
foreign import capi "jack_port_monitoring_input"
  c_jackPortMonitoringInput :: JackPort -> IO CInt

-- Function to establish a connection between two ports.
-- When a connection exists, data written to the source port will
-- be available to be read at the destination port.
-- @pre The port types must be identical.
-- @pre The source_port must include JackPortIsOutput.
-- @pre The destination_port must include JackPortIsInput.
-- @return 0 on success, EEXIST if the connection is already made, otherwise a non-zero error code
foreign import capi "jack/jack.h  jack_connect"
  c_jackConnect :: JackClient -> CString -> CString -> IO CInt

-- Function to remove a connection between two ports.
-- @pre The port types must be identical.
-- @pre The source_port must include JackPortIsOutput.
-- @pre The destination_port must include JackPortIsInput.
-- @return 0 on success, otherwise a non-zero error code
foreign import capi "jack/jack.h  jack_disconnect"
  c_jackDisconnect :: JackClient -> CString -> CString -> IO CInt

-- Function to disconnect a port using port handles rather than names.
-- This avoids the name lookup inherent in the name-based version.
-- Clients connecting their own ports are likely to use this function.
foreign import capi "jack/jack.h  jack_port_disconnect"
  c_jackPortDisconnect :: JackClient -> JackPort -> IO CInt

-- Function to get the maximum number of characters in a full JACK port name.
-- @return the maximum number of characters in a full JACK port name including the final NULL character.
foreign import capi "jack/jack.h jack_port_name_size"
  c_jackPortNameSize :: IO CInt

-- Function to get the maximum number of characters in a JACK port type name.
-- @return the maximum number of characters in a JACK port type name including the final NULL character.
foreign import capi "jack/jack.h jack_port_type_size"
  c_jackPortTypeSize :: IO CInt

foreign import capi "jack/jack.h  jack_port_type_get_buffer_size"
  c_jackPortTypeGetBufferSize :: JackClient -> CString -> IO CSize

-- ! Additional helper functions and data handling might be necessary for proper usage.

-- | Managing and determining latency
-- | Allows clients to answer:
-- | - Time since data read from a port arrived at the edge of the JACK graph.
-- | - Time until data written to a port arrives at the edge of the JACK graph.
-- | Ports have two latency values: capture latency and playback latency, typically expressed as a min/max pair.

-- | Set the latency of a port. Clients controlling physical hardware should set this to include systemic latency.
-- | @deprecated Use jack_port_set_latency_range instead.
foreign import capi "jack/jack.h jack_port_set_latency"
  c_jackPortSetLatency :: JackPort -> JackNFrames -> IO ()

-- | Get the latency range defined by @a mode for @a port, in frames.
-- | Best used from callbacks, specifically the latency callback.
foreign import capi "jack/jack.h jack_port_get_latency_range"
  c_jackPortGetLatencyRange :: JackPort -> JackLatencyCallbackMode -> JackLatencyRange -> IO ()

-- | Set the minimum and maximum latencies for a port. Should only be used inside a latency callback.
foreign import capi "jack/jack.h jack_port_set_latency_range"
  c_jackPortSetLatencyRange :: JackPort -> JackLatencyCallbackMode -> JackLatencyRange -> IO ()

-- | Request a complete recomputation of all port latencies after changes.
foreign import capi "jack/jack.h jack_recompute_total_latencies"
  c_jackRecomputeTotalLatencies :: JackClient -> IO CInt

-- | Get the current latency of a port. This is the time between data availability and it arriving or being delivered.
-- | @deprecated Use jack_port_get_latency_range instead.
foreign import capi "jack/jack.h jack_port_get_latency"
  c_jackPortGetLatency :: JackPort -> IO JackNFrames

-- | Get the total latency of a port, which is the maximum of the sum of latencies in every connection path.
-- | @deprecated Use jack_port_get_latency_range instead.
foreign import capi "jack/jack.h jack_port_get_total_latency"
  c_jackPortGetTotalLatency :: JackClient -> JackPort -> IO JackNFrames

-- | Recompute a port's total latency. Call this after changing internal latency to update all signal pathways.
-- | @deprecated Use jack_recompute_total_latencies instead.
foreign import capi "jack/jack.h jack_recompute_total_latency"
  c_jackRecomputeTotalLatency :: JackClient -> JackPort -> IO CInt

-- | Port Searching
-- | Get a list of port names matching certain criteria.
foreign import capi "jack/jack.h jack_get_ports"
  c_jackGetPorts :: JackClient -> CString -> CString -> CULong -> IO (Ptr CString)

-- | Get a port by its name.
foreign import capi "jack/jack.h jack_port_by_name"
  c_jackPortByName :: JackClient -> CString -> IO JackPort

-- | Get a port by its ID.
foreign import capi "jack/jack.h jack_port_by_id"
  c_jackPortById :: JackClient -> JackPortId -> IO (JackPort)

-- | Time Functions
-- | Get the number of frames since the start of the current cycle.
foreign import capi "jack/jack.h jack_frames_since_cycle_start"
  c_jackFramesSinceCycleStart :: Ptr JackClient -> IO JackNFrames

-- | Get the current frame number.
foreign import capi "jack/jack.h jack_frame_time"
  c_jackFrameTime :: Ptr JackClient -> IO JackNFrames

-- | Get the exact frame time at the start of the current process cycle.
foreign import capi "jack/jack.h jack_last_frame_time"
  c_jackLastFrameTime :: Ptr JackClient -> IO JackNFrames

-- | Convert frames to time in microseconds.
foreign import capi "jack/jack.h jack_frames_to_time"
  c_jackFramesToTime :: JackClient -> JackNFrames -> IO JackTime

-- | Convert system time in microseconds to frames.
foreign import capi "jack/jack.h jack_time_to_frames"
  c_jackTimeToFrames :: JackClient -> JackTime -> IO JackNFrames

-- | Get the current system time in microseconds.
foreign import capi "jack/jack.h jack_get_time"
  jackGetTime :: IO JackTime

-- | Error and Info Output Handling
-- | Set custom functions for handling JACK error messages.
foreign import capi "jack/jack.h jack_set_error_function"
  jackSetErrorFunction :: FunPtr (CString -> IO ()) -> IO ()

-- | Set custom functions for handling JACK info messages.
foreign import capi "jack/jack.h jack_set_info_function"
  jackSetInfoFunction :: FunPtr (CString -> IO ()) -> IO ()

-- | Free memory allocated by JACK functions.
foreign import capi "jack/jack.h jack_free"
  jackFree :: Ptr a -> IO ()

-- !! Usage of these functions should handle potential NULL pointers
-- !! and manage memory where necessary, especially when dealing with strings returned by JACK.

-- Connect ports
foreign import capi unsafe "jack/jack.h  jack_connect"
  c_jack_connect :: Ptr JackClient -> CString -> CString -> IO CInt

-- Disconnect ports
foreign import capi unsafe "jack/jack.h  jack_disconnect"
  c_jack_disconnect :: Ptr JackClient -> CString -> CString -> IO CInt

-- Get all ports that match the specified criteria
foreign import capi unsafe " jack/jack.h  jack_get_ports"
  c_jack_get_ports :: Ptr JackClient -> CString -> CString -> CULong -> IO (Ptr CString)

-- Port-related operations
foreign import capi unsafe "jack/jack.h  jack_port_get_buffer"
  c_jack_port_get_buffer :: Ptr JackPort -> CUInt -> IO (Ptr ())


