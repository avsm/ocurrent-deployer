(* Compose configuration for a Caddy HTTPS server on a particular domain/ip *)

type t = {
  name: string;
  domains: string list;
  ip: string;
}

let caddy_service { name; domains; ip; _ } =
  let domains = domains |> List.map (fun d -> "--domain " ^ d) |> String.concat " " in
  let service = Fmt.strf {|
  %s:
    image: $IMAGE_HASH
    command: --domain %s --root /usr/share/caddy
    restart: always
    networks:
    - %s_network
    ports:
    - target: 80
      published: 80
      protocol: tcp
    - target: 443
      published: 443
      protocol: tcp
|} name domains name
  in
  let network = Fmt.strf {|
  %s_network:
    driver_opts:
        com.docker.network.bridge.host_binding_ipv4: "%s"
|} name ip
  in
  service, network

let compose ~sites () =
  sites |>
  List.map caddy_service |>
  List.fold_left (fun (s,n) (service, network) -> (s ^ service), (n ^ network)) ("","") |>
  fun (services, networks) ->
  Fmt.strf {|
version: "3.7"
services:%s
networks:%s
|} services networks

(* Replace $IMAGE_HASH in the compose file with the fixed (hash) image id *)
let replace_hash_var ~hash contents =
  Re.Str.(global_replace (regexp_string "$IMAGE_HASH") hash contents)
