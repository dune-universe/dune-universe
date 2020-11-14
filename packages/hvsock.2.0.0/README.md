## ocaml-hvsock -- bindings for Hypervisor sockets

[![Build Status (Linux)](https://travis-ci.org/mirage/ocaml-hvsock.svg)](https://travis-ci.org/mirage/ocaml-hvsock)
[![Build status (Windows)](https://ci.appveyor.com/api/projects/status/974tsg317b4k8xra?svg=true)](https://ci.appveyor.com/project/mirage/ocaml-hvsock/branch/master)

These bindings allow Host <-> VM communication
- on Windows with Hyper-V sockets
- on Linux with `AF_VSOCK`
- on macOS with VMs running with [hyperkit](https://github.com/moby/hyperkit).

Please read [the API documentation](https://mirage.github.io/ocaml-hvsock/index.html)
and see the example [src/hvcat.ml](src/hvcat.ml).


# Addressing

The addressing scheme used is hypervisor-dependent. Windows identifies a VM (partition)
with a GUID and a service with a GUID. Linux identifies a VM with a 32-bit int and a
port as a 32-bit int. When running on a Windows host, the Linux kernel maps a 32-bit
port number onto a special range of Hyper-V socket GUIDs using the format string
`"%08lx-FACB-11E6-BD58-64006A7986D3"`.

This library exposes the underlying addressing schemes in the modules
- `Hvsock.Af_hyperv`: corresponding to the `AF_HYPERV` address family on Windows
- `Hvsock.Af_vsock`: corresponding to the `AF_VSOCK` address family on Linux
- `Hvsock.Hyperkit`: corresponding to the macOS backend implementation of the
  `AF_VSOCK` address family on Linux.

For utilities that wish to be cross-platform, there is a generic interface in
`Hvsock.Socket` which understands addresses encoded as URIs, suitable for command-line
interfaces (and compatible with those used by
[linuxkit/virtsock](https://github.com/linuxkit/virtsock)). The URIs look like:

- `vsock://:80`: listen on `AF_VSOCK` port 80. If run in a Linux VM on a Windows host
  then the Windows service GUID will be `00000050-FACB-11E6-BD58-64006A7986D3`.
- `hvsock://<VM GUID>/3049197C-9A4E-4FBF-9367-97F792F16994`: connect to service `3049..`
  on a Windows VM `<VM GUID>` as displayed by the powershell `(Get-VM Name).Id`. This
  requires the service `3049...` to be pre-registered in the registry but the connection
  itself can be run as a non-Administrator user.
- `hvsock://<VM name>/3049197C-9A4E-4FBF-9367-97F792F16994`: connect to a service `3049..`
  in the Windows VM `<VM name>`. Note the library will shell out to powershell to resolve
  the name to a GUID, which unfortunately requires Administrator.
- `vsock://2:80/`: connect to a Linux VM with id `2` on port `80` via `AF_VSOCK`
- `hyperkit://:80/Users/foo/Library/Containers/com.docker.docker/Data/vms/0`: connect to
  `AF_VSOCK` port 80 on the Hyperkit VM running at the given filesystem path.

# Example

Consider the Linux VM running on a Windows host started by
[Docker Desktop](https://www.docker.com/). The VM name in Hyper V manager is
`MobyLinuxVM`.

First build and run this code in a privileged Linux container in the VM:
```
docker build -t hvsock .
docker run -it -v <path>:/src --privileged hvsock sh
make
./_build/default/src/hvcat.exe -l vsock://:255
```

Next build and run the code on the host:
```
make
./_build/default/src/hvcat hvsock://MobyLinuxVM/000000ff-FACB-11E6-BD58-64006A7986D3 --register
```

The `--register` flag will run a powershell fragment to register the serviceid in the
registry. This requires Administrator but only needs to be done once.

# Limitations

Although the connections use the regular socket APIs, current Windows kernels
don't support calls like `select` so we must always use blocking I/O from background threads, rather than regular asynchronous I/O. The `Hvsock_lwt.Socket` module can
create a Lwt-compatible I/O layer on top of regular threads.

The client `connect` call seems to block forever if the server calls `listen`
after the client calls `connect`. The `Af_hyperv.connect` works around this
with a self-imposed 1s timeout after which time it will raise `ECONNREFUSED`.

Depending on the Windows and Linux kernel versions, in-flight data can be lost after
a `shutdown_write` or `close` on a Windows host.

# Background

For background, see the [MSDN article on making an integration service](https://msdn.microsoft.com/en-us/virtualization/hyperv_on_windows/develop/make_mgmt_service)
